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
      def run, do: Assertion.Test.run(@tests, __MODULE__)
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
      Assertion.Test.assert(operator, lhs, rhs)
    end
  end

  defmacro assert(boolean) do
    Assertion.Test.assert(boolean)
  end

  defmacro refute({operator, _, [lhs, rhs]}) do
    quote bind_quoted: [operator: operator, lhs: lhs, rhs: rhs] do
      Assertion.Test.refute(operator, lhs, rhs)
    end
  end

  defmacro refute(boolean) do
    Assertion.Test.refute(boolean)
  end

end

defmodule Assertion.Test do
  def run(tests, module) do
    Enum.each tests, fn {test_func, description} ->
      case apply(module, test_func, []) do
        :ok             -> IO.write ".\n"
        {:fail, reason} -> IO.puts """

          ===============================================
          FAILURE: #{description}
          ===============================================
          #{reason}
          """
      end
    end
  end

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
