defmodule Assertion do

  @doc """
  On using the module it imports all the macros on this module and registers a
  module attributed called tests on which all the tests are accumulated. As last
  step it triggers the before_compile function that will define a run test for
  executing all the tests.
  """
  defmacro __using__(_options) do
    quote do
      import unquote(__MODULE__)
      Module.register_attribute __MODULE__, :tests, accumulate: true
      @before_compile unquote(__MODULE__)
    end
  end

  @doc """
  This function defines the run function on the module that imports it. This is
  done before compilation. When called it starts the tests and times the
  execution. It outputs the result, failures and the amount of time passed.
  """
  defmacro __before_compile__(_env) do
    quote do
      def run do
        { time, result } = :timer.tc(Assertion.Test, :run, [ @tests, __MODULE__ ])
        %{ ok: passed, fail: failed, errors: errors, result: output } = result |> count_results
        IO.write "\n"
        IO.puts output
        IO.write "\n"
        IO.puts Enum.join(errors)
        :io.format "Execution time (ms): ~.2f~n", [ time / 1000.0 ]
        :io.format "Passed: ~B~nFailed: ~B~n", [ passed, failed ]
      end
    end
  end

  @doc """
  This function defines the test macro which receives a description and a block.
  The description is turned into an Atom for the function name. The test
  function is stored in the module attribute @tests. This way there is a
  reference to all the tests. The test function is then defined on the module.
  This way the test can be executed by calling it using the description as the
  name.
  """
  defmacro test(description, do: test_block) do
    test_func = String.to_atom(description)
    quote do
      @tests {unquote(test_func), unquote(description)}
      def unquote(test_func)(), do: unquote(test_block)
    end
  end

  defmacro assert({operator, _context, [lhs, rhs]}) do
    quote bind_quoted: [operator: operator, lhs: lhs, rhs: rhs] do
      Assertion.Asserts.assert(operator, lhs, rhs)
    end
  end

  defmacro assert(expression) do
    quote bind_quoted: [expression: expression] do
      Assertion.Asserts.assert(expression)
    end
  end

  defmacro refute({operator, _context, [lhs, rhs]}) do
    quote bind_quoted: [operator: operator, lhs: lhs, rhs: rhs] do
      Assertion.Refutes.refute(operator, lhs, rhs)
    end
  end

  defmacro refute(expression) do
    quote bind_quoted: [expression: expression] do
      Assertion.Refutes.assert(expression)
    end
  end

  @doc """
  This function takes the result of running all the tests and transforms it into
  a struct that has the count of passed and failed tests, all the errors and
  the result for a nice output.
  """
  def count_results(result) do
    result |> Enum.reduce(%{ fail: 0, ok: 0, result: "", errors: [] }, fn (res, acc) ->
      case res do
        { :ok } ->
          { _, acc } = Map.get_and_update(acc, :result, fn result ->
            { result, result <> "#{IO.ANSI.green}.#{IO.ANSI.default_color}" }
          end)
          { _, acc } = Map.get_and_update(acc, :ok, fn count -> { count, count + 1 } end)
        { :fail, msg } ->
          { _, acc } = Map.get_and_update(acc, :result, fn result ->
            { result, result <> "#{IO.ANSI.red}F#{IO.ANSI.default_color}" }
          end)
          { _, acc } = Map.get_and_update(acc, :fail, fn count -> { count, count + 1 } end)
          { _, acc } = Map.get_and_update(acc, :errors, fn errors -> { errors, [ msg | errors ] } end )
      end
      acc
    end)
  end
end

defmodule Assertion.Test do
  @doc """
  This function spawns a worker for each test it receives and starts all the
  workers for parallel execution of the tests. The number of workers is based
  on the number of tests passed.
  """
  def run(tests, module) do
    number_of_workers = length(tests)
    1..number_of_workers
    |> Enum.map(fn _ -> spawn_link(__MODULE__, :run_worker, [ self ]) end)
    |> run_workers(tests, module, [])
  end

  @doc """
  This function runs the worker. On start it sends out the ready message to
  indicate it is ready to receive work. When the :test message is received it
  runs the test and sends the result to the parent PID. For the :shutdown
  message we exit normally and thus killing the process.
  """
  def run_worker(parent) do
    send parent, { :ready, self }
    receive do
      { :test, module, function, description } ->
        send parent, run_test(module, function, description)
        run_worker(parent)
      { :shutdown } ->
        exit(:normal)
    end
  end

  @doc """
  This applies the test function to the module passed. If the function returns
  a fail message its reason is returned along with the description for
  reference. Else just the OK tuple is returned.
  """
  def run_test(module, test_func, description) do
    case apply(module, test_func, []) do
        :ok             -> { :ok, "." }
        {:fail, reason} -> { :fail, """
          #{IO.ANSI.red}\
          ===============================================
          FAILURE: #{description}
          ===============================================
          #{IO.ANSI.default_color}\
          #{reason}
          """ }
      end
  end

  @doc """
  This function listens for message from the workers. If a worker sends out the
  :ready message with it's PID it gets a message to perform the given test
  function. The test is then remove from the queue and the function calls
  itself recursively to listen for the next incomming message. If the ready
  message is retrieved and all the arguments are parsed it sends out the
  shutdown message and removes the PID from the list of workers. This cleans
  up unused workers and eventually, if all work is done, all the workers.  The
  later sitation is because the worker always send :ready after each execution.
  If the message containing :result is received it adds the result to the
  results accumulator and calls itself recursively.
  The code is based on what I've learned in Programming Elixir 1.2 book chapter
  13 exercise 9.
  """
  def run_workers(workers, tests, module, results) do
    receive do
      { :ready, pid } when length(tests) > 0 ->
        [ { test, description } | tail ] = tests
        send pid, { :test, module, test, description }
        run_workers(workers, tail, module, results)
      { :ready, pid } ->
        send pid, { :shutdown }
        if length(workers) > 1 do
          run_workers(List.delete(workers, pid), tests, module, results)
        else
          results
        end
      { :ok, _message } ->
        results = [ { :ok } | results ]
        run_workers(workers, tests, module, results)
      { :fail, message } ->
        results = [ { :fail, message } | results ]
        run_workers(workers, tests, module, results)
      msg ->
        IO.puts "Unknown response: #{inspect msg}"
    end
  end
end

defmodule Assertion.Asserts do
  def assert(true), do: :ok
  def assert(false) do
    {:fail, """
      Expected value to be truthy
      """
    }
  end

  def assert(:==, lhs, rhs) when lhs == rhs, do: :ok
  def assert(:==, lhs, rhs) do
    {:fail, """
      Expected:       #{lhs}
      to be equal to: #{rhs}
      """
    }
  end

  def assert(:>, lhs, rhs) when lhs > rhs, do: :ok
  def assert(:>, lhs, rhs) do
    {:fail, """
      Expected:           #{lhs}
      to be greater than: #{rhs}
      """
    }
  end

  def assert(:<, lhs, rhs) when lhs < rhs, do: :ok
  def assert(:<, lhs, rhs) do
    {:fail, """
      Expected:           #{lhs}
      to be lesser than: #{rhs}
      """
    }
  end

  def assert(:<=, lhs, rhs) when lhs <= rhs, do: :ok
  def assert(:<=, lhs, rhs) do
    {:fail, """
      Expected:                      #{lhs}
      to be lesser than or equal to: #{rhs}
      """
    }
  end

  def assert(:>=, lhs, rhs) when lhs >= rhs, do: :ok
  def assert(:>=, lhs, rhs) do
    {:fail, """
      Expected:                       #{lhs}
      to be greater than or equal to: #{rhs}
      """
    }
  end

  def assert(:!=, lhs, rhs) when lhs != rhs, do: :ok
  def assert(:!=, lhs, rhs) do
    {:fail, """
      Expected:        #{lhs}
      to not equal to: #{rhs}
      """
    }
  end

  def assert(:===, lhs, rhs) when lhs === rhs, do: :ok
  def assert(:===, lhs, rhs) do
    {:fail, """
      Expected:           #{lhs}
      to strict equal to: #{rhs}
      """
    }
  end

  def assert(:!==, lhs, rhs) when lhs !== rhs, do: :ok
  def assert(:!==, lhs, rhs) do
    {:fail, """
      Expected:               #{lhs}
      to strict not equal to: #{rhs}
      """
    }
  end
end

defmodule Assertion.Refutes do
  def refute(false), do: :ok
  def refute(true) do
    {:fail, """
      Not expected value to be truthy
      """
    }
  end

  def refute(:==, lhs, rhs) when lhs == rhs do
    {:fail, """
      Expected:       #{lhs}
      not to be equal to: #{rhs}
      """
    }
  end
  def refute(:==, _lhs, _rhs), do: :ok

  def refute(:>, lhs, rhs) when lhs > rhs do
    {:fail, """
      Expected:           #{lhs}
      not to be greater than: #{rhs}
      """
    }
  end
  def refute(:>, _lhs, _rhs), do: :ok

  def refute(:<, lhs, rhs) when lhs < rhs do
    {:fail, """
      Expected:           #{lhs}
      not to be lesser than: #{rhs}
      """
    }
  end
  def refute(:<, _lhs, _rhs), do: :ok

  def refute(:<=, lhs, rhs) when lhs <= rhs do
    {:fail, """
      Expected:                      #{lhs}
      not to be lesser than or equal to: #{rhs}
      """
    }
  end
  def refute(:<=, _lhs, _rhs), do: :ok

  def refute(:>=, lhs, rhs) when lhs >= rhs do
    {:fail, """
      Expected:                       #{lhs}
      not to be greater than or equal to: #{rhs}
      """
    }
  end
  def refute(:>=, _lhs, _rhs), do: :ok

  def refute(:!=, lhs, rhs) when lhs != rhs do
    {:fail, """
      Expected:        #{lhs}
      not to not equal to: #{rhs}
      """
    }
  end
  def refute(:!=, _lhs, _rhs), do: :ok

  def refute(:===, lhs, rhs) when lhs === rhs do
    {:fail, """
      Expected:               #{lhs}
      not to strict equal to: #{rhs}
      """
    }
  end
  def refute(:===, _lhs, _rhs), do: :ok

  def refute(:!==, lhs, rhs) when lhs !== rhs do
    {:fail, """
      Expected:                   #{lhs}
      not to strict not equal to: #{rhs}
      """
    }
  end
  def refute(:!==, _lhs, _rhs), do: :ok
end

defmodule MathTest do
  use Assertion

  test "assert [SUCCESS] equal to or greater than", do: assert 1 >= 1
  test "assert [FAIL] equal to or greater than", do: assert -1 >= 0

  test "assert [SUCCESS] equal to or lesser than", do: assert 1 <= 1
  test "assert [FAIL] equal to or lesser than", do: assert 2 <= 1

  test "assert [SUCCESS] not equal to", do: assert 1 != 2
  test "assert [FAIL] not equal to", do: assert 1 != 1

  test "assert [SUCCESS] strict equal to", do: assert 1 === 1
  test "assert [FAIL] strict equal to", do: assert 1 === 1.0

  test "assert [SUCCESS] strict not equal to", do: assert 1 !== 2
  test "assert [FAIL] strict not equal to", do: assert 1 !== 1

  test "assert [SUCCESS] equal to", do: assert 1 == 1
  test "assert [FAIL] equal to", do: assert 1 == 2

  test "assert [SUCCESS] greater than", do: assert 2 > 1
  test "assert [FAIL] greater than", do: assert 1 > 2

  test "assert [SUCCESS] lesser to", do: assert 1 < 2
  test "assert [FAIL] lesser to", do: assert 2 < 1

  test "assert [SUCCESS] asserting boolean", do: assert true
  test "assert [FAIL] asserting boolean", do: assert false


  test "refute [FAIL] equal to or greater than", do: refute 1 >= 1
  test "refute [SUCCESS] equal to or greater than", do: refute -1 >= 0

  test "refute [FAIL] equal to or lesser than", do: refute 1 <= 1
  test "refute [SUCCESS] equal to or lesser than", do: refute 2 <= 1

  test "refute [FAIL] not equal to", do: refute 1 != 2
  test "refute [SUCCESS] not equal to", do: refute 1 != 1

  test "refute [FAIL] strict equal to", do: refute 1 === 1
  test "refute [SUCCESS] strict equal to", do: refute 1 === 1.0

  test "refute [FAIL] strict not equal to", do: refute 1 !== 2
  test "refute [SUCCESS] strict not equal to", do: refute 1 !== 1

  test "refute [FAIL] equal to", do: refute 1 == 1
  test "refute [SUCCESS] equal to", do: refute 1 == 2

  test "refute [FAIL] greater than", do: refute 2 > 1
  test "refute [SUCCESS] greater than", do: refute 1 > 2

  test "refute [FAIL] lesser to", do: refute 1 < 2
  test "refute [SUCCESS] lesser to", do: refute 2 < 1

  test "refute [FAIL] refuteing boolean", do: refute true
  test "refute [SUCCESS] refuteing boolean", do: refute false
end
