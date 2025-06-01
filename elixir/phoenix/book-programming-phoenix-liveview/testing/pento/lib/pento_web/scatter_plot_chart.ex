#---
# Excerpted from "Programming Phoenix LiveView",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material,
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose.
# Visit https://pragprog.com/titles/liveview for more book information.
#---
defmodule PentoWeb.ScatterPlotChart do
  alias Contex.{Dataset, Plot, PointPlot}

  def new_scatter_plot_dataset(categories) do
    [
      [NaiveDateTime.local_now() | Enum.map(categories, fn _name -> 0 end)]
    ]
    |> Dataset.new(scatter_plot_headers(categories))
  end

  @spec update_scatter_plot_dataset(any, atom | %{:data => list, optional(any) => any}, %{
          :category => any,
          :timestamp => any,
          :value => number,
          optional(any) => any
        }) :: Contex.Dataset.t()
  def update_scatter_plot_dataset(categories, dataset, new_plot_point) do
    Dataset.new(
      updated_data(categories, dataset, new_plot_point),
      scatter_plot_headers(categories)
    )
  end

  def make_scatter_plot_chart(dataset, categories) do
    PointPlot.new(dataset, mapping: categories)
  end

  def render_scatter_plot_chart(chart, title, subtitle, x_axis, y_axis) do
    Plot.new(600, 400, chart)
    |> Plot.titles(title, subtitle)
    |> Plot.axis_labels(x_axis, y_axis)
    |> Plot.plot_options(%{legend_setting: :legend_right})
    |> Plot.to_svg()
  end

  defp updated_data(categories, dataset, %{
         category: category,
         value: value,
         timestamp: timestamp
       }) do
    value_index = Enum.find_index(categories, fn x -> x == category end) + 1
    data = dataset.data

    new_point =
      List.last(data)
      |> List.replace_at(0, timestamp)
      |> new_value(value_index, value)

    data ++ [new_point]
  end

  defp new_value(plot_point, value_index, value) do
    new_value = Enum.at(plot_point, value_index) + value
    List.replace_at(plot_point, value_index, new_value)
  end

  defp scatter_plot_headers(categories) do
    ["X" | categories]
  end
end
