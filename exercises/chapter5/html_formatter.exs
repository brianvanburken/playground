defmodule HtmlFormatter do
  @indent "  "

  def format(tags), do: tags |> do_format(:none, "", 0)

  defp do_format([], _previous_type, acc, _depth), do: acc
  defp do_format([ tag ], _previous_type, acc, _depth), do: acc <> tag
  defp do_format([ tag, next | tail ], previous_type, acc, depth) do
    current_tag_type = tag_type(tag)
    next_tag_type = tag_type(next)
    case current_tag_type do
      :opening ->
        [
          acc,
          indent(depth),
          tag,
          ( case next_tag_type do
            :opening -> "\n"
            :closing -> ""
            :none    -> ""
          end ),
          do_format([ next | tail], current_tag_type, acc, depth + 1),
        ] |> Enum.join
      :closing ->
        [
          acc,
          ( case previous_type do
            :opening -> ""
            :none    -> ""
            _        -> indent(depth - 1)
          end ),
          tag,
          ( case next_tag_type do
            :opening -> "\n"
            :closing -> "\n"
            :none    -> ""
          end ),
          do_format([ next | tail ], current_tag_type, acc, depth - 1),
        ] |> Enum.join
      :none ->
        [
          acc,
          tag,
          do_format([ next | tail ], current_tag_type, acc, depth)
        ] |> Enum.join
    end
  end

  defp indent(depth), do: String.duplicate(@indent, depth)

  defp tag_type(tag) do
    case Regex.run(~r/^<\/?/, tag) do
      ["<"]  -> :opening
      ["</"] -> :closing
      _      -> :none
    end
  end
end
