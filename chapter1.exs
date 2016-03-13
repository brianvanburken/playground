defmodule My do
  defmacro unless(condition, clauses) do
    do_clause = Keyword.get(clauses, :do, nil)
    else_clause = Keyword.get(clauses, :else, nil)
    quote do
      case unquote(condition) do
        val when val in [false, nil] -> unquote(do_clause)
        _                            -> unquote(else_clause)
      end
    end
  end
end

defmodule Test do

  def my_unless do
    import Kernel, except: [ unless: 2 ]
    import My

    unless 1 == 2 do
      IO.puts "1 != 2"
    else
      IO.puts "1 == 2"
    end
  end

  def my_ast do
    import Kernel, except: [ unless: 2 ]
    import My

    ast = {
      :unless, [context: Elixir, import: My], [
        { :var!, [context: Elixir, import: Kernel], [ {:test, [], Elixir} ] },
        [do: "true", else: "false"]
      ]
    }

    Code.eval_quoted ast, test: true
  end
end
