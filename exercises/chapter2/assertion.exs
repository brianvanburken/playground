defmodule Assertion do

  defmacro __using__(_options) do
    quote do
      import unquote(__MODULE__)
      Module.register_attribute __MODULE__, :tests, accumulate: true
      @before_compile unquote(__MODULE__)
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      def run do
        { time, result } = :timer.tc( Assertion.Test, :run, [ @tests, __MODULE__ ])
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

  defmacro test(description, do: test_block) do
    test_func = String.to_atom(description)
    quote do
      @tests {unquote(test_func), unquote(description)}
      def unquote(test_func)(), do: unquote(test_block)
    end
  end

  defmacro assert({operator, _, [lhs, rhs]}) do
    quote bind_quoted: [operator: operator, lhs: lhs, rhs: rhs] do
      Assertion.Asserts.assert(operator, lhs, rhs)
    end
  end

  defmacro assert(boolean) do
    Assertion.Asserts.assert(boolean)
  end

  defmacro refute({operator, _, [lhs, rhs]}) do
    quote bind_quoted: [operator: operator, lhs: lhs, rhs: rhs] do
      Assertion.Refutes.refute(operator, lhs, rhs)
    end
  end

  defmacro refute(boolean) do
    Assertion.Refutes.refute(boolean)
  end

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
  def run(tests, module) do
    number_of_workers = length(tests)
    1..number_of_workers
    |> Enum.map(fn _ -> spawn_link(__MODULE__, :run_worker, [ self ]) end)
    |> run_workers(tests, module, [])
  end

  def run_worker(parent) do
    send parent, { :ready, self }
    receive do
      { :test, module, function, description } ->
        send parent, run_test(module, function, description)
        run_worker(parent)
      { :terminate } ->
        exit(:normal)
    end
  end

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

  def run_workers(workers, tests, module, results) do
    receive do
      { :ready, pid } when length(tests) > 0 ->
        [ { test, description } | tail ] = tests
        send pid, { :test, module, test, description }
        run_workers(workers, tail, module, results)
      { :ready, pid } ->
        send pid, { :terminate }
        if length(workers) > 1 do
          run_workers(List.delete(workers, pid), tests, module, results)
        else
          results
        end
      { :ok, _message } ->
        results = [ { :ok } | results ]
        run_workers(workers, tests, module, results)
      { :fail, reason } ->
        results = [ { :fail, reason } | results ]
        run_workers(workers, tests, module, results)
      msg ->
        IO.puts "Unknown response: #{inspect msg}"
    end
  end
end

defmodule Assertion.Asserts do
  def assert(true), do: :ok
  def assert(_) do
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
  def refute(_) do
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
