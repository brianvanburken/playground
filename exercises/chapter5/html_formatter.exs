defmodule HtmlFormatter do
  @indent "  "

  def format(tags), do: tags |> do_format(:none, "", 0)

  @doc """
  This method indents the list of tags based on their hierarchical depth. Its
  checks are based on the current and next node type. If the current node is an
  opening node it adds an indent for the current depth. A new line is added only
  if the next node is also an opening node. For closing tags it decreases the
  indent and adds a new line. For all other it just adds the node to the
  accumulator. This is done recursivly untill in the end we build a list of
  nodes and indentations. This is then joined and returned as a string.
  """
  defp do_format([], _previous_type, acc, _depth), do: acc
  defp do_format([ node ], _previous_type, acc, _depth), do: acc <> node
  defp do_format([ node, next | tail ], previous_type, acc, depth) do
    current_tag_type = node_type(node)
    next_tag_type = node_type(next)
    case current_tag_type do
      :opening_tag ->
        [
          acc,
          indent(depth),
          node,
          ( case next_tag_type do
            :opening_tag -> "\n"
                       _ -> ""
          end ),
          do_format([ next | tail], current_tag_type, acc, depth + 1),
        ]
      :closing_tag ->
        [
          acc,
          ( case previous_type do
            :closing_tag -> indent(depth - 1)
                       _ -> ""
          end ),
          node,
          "\n",
          do_format([ next | tail ], current_tag_type, acc, depth - 1),
        ]
      :text ->
        [
          acc,
          node,
          do_format([ next | tail ], current_tag_type, acc, depth)
        ]
    end
  end

  @doc """
  Given the depth, it generates a string of indentation characters. Identation
  is defined in the module attribute @indent.
  """
  defp indent(depth), do: String.duplicate(@indent, depth)

  @doc """
  Checks if the node passed starts with a '<' for starting tags, '</' for
  closing tags. Else it is defined as a text node.
  """
  defp node_type(node) do
    case Regex.run(~r/^<\/?/, node) do
      ["<"]  -> :opening_tag
      ["</"] -> :closing_tag
      _      -> :text
    end
  end
end
