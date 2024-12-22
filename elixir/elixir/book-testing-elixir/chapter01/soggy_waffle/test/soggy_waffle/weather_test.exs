defmodule SoggyWaffle.WeatherTest do
  use ExUnit.Case
  alias SoggyWaffle.Weather

  describe "imminent_rain?/2" do
    test "returns true when it will rain in the future" do
      now = datetime_struct(hour: 0, minute: 0, second: 0)
      one_second_from_now = datetime_struct(hour: 0, minute: 0, second: 1)
      weather_data = [weather_struct(one_second_from_now, :rain)]

      assert Weather.imminent_rain?(weather_data, now) == true
    end

    test "returns false for empty weather data list" do
      now = datetime_struct(hour: 0, minute: 0, second: 0)
      weather_data = []

      refute Weather.imminent_rain?(weather_data, now)
    end

    test "returns false if there is no rain in the data" do
      now = datetime_struct(hour: 0, minute: 0, second: 0)

      weather_data = [
        weather_struct(
          datetime_struct(hour: 1, minute: 0, second: 0),
          :no_rain
        )
      ]

      refute Weather.imminent_rain?(weather_data, now)
    end

    test "returns false if it rains but not within the next 4 hours" do
      now = datetime_struct(hour: 0, minute: 0, second: 0)
      five_hours_later = datetime_struct(hour: 5, minute: 0, second: 0)

      weather_data = [
        weather_struct(five_hours_later, :rain)
      ]

      refute Weather.imminent_rain?(weather_data, now)
    end

    test "returns true if it rains within the next 4 hours" do
      now = datetime_struct(hour: 0, minute: 0, second: 0)
      two_hours_later = datetime_struct(hour: 2, minute: 0, second: 0)

      weather_data = [
        weather_struct(two_hours_later, :rain)
      ]

      assert Weather.imminent_rain?(weather_data, now)
    end

    test "returns true if rain is exactly at the 4-hour mark" do
      now = datetime_struct(hour: 0, minute: 0, second: 0)
      four_hours_later = datetime_struct(hour: 4, minute: 0, second: 0)

      weather_data = [
        weather_struct(four_hours_later, :rain)
      ]

      assert Weather.imminent_rain?(weather_data, now)
    end

    test "returns true if multiple data points exist and at least one has rain within 4 hours" do
      now = datetime_struct(hour: 0, minute: 0, second: 0)

      weather_data = [
        weather_struct(datetime_struct(hour: 6, minute: 0, second: 0), :rain),
        weather_struct(datetime_struct(hour: 2, minute: 0, second: 0), :rain)
      ]

      assert Weather.imminent_rain?(weather_data, now)
    end
  end

  test "returns false if all data is in the past" do
    now = datetime_struct(hour: 4, minute: 0, second: 0)
    one_hour_ago = datetime_struct(hour: -3, minute: 0, second: 0)
    two_hours_ago = datetime_struct(hour: -2, minute: 0, second: 0)

    weather_data = [
      weather_struct(one_hour_ago, :rain),
      weather_struct(two_hours_ago, :rain)
    ]

    refute Weather.imminent_rain?(weather_data, now)
  end

  test "returns true if there's a mix of past and future data (with rain in next 4 hours)" do
    now = datetime_struct(hour: 0, minute: 0, second: 0)
    past_time = datetime_struct(hour: 0, minute: 0, second: -1)
    three_hours_later = datetime_struct(hour: 3, minute: 0, second: 0)

    weather_data = [
      weather_struct(past_time, :rain),
      weather_struct(three_hours_later, :rain)
    ]

    assert Weather.imminent_rain?(weather_data, now)
  end

  test "returns true if rain event is exactly at now" do
    now = datetime_struct(hour: 0, minute: 0, second: 0)
    same_as_now = datetime_struct(hour: 0, minute: 0, second: 0)

    weather_data = [
      weather_struct(same_as_now, :rain)
    ]

    assert Weather.imminent_rain?(weather_data, now)
  end

  test "defaults to now" do
    now = DateTime.utc_now() |> DateTime.add(5, :second)

    weather_data = [
      weather_struct(now, :rain)
    ]

    assert Weather.imminent_rain?(weather_data)
  end

  defp weather_struct(datetime, condition) do
    %Weather{
      datetime: datetime,
      rain?: condition == :rain
    }
  end

  defp datetime_struct(opts) do
    %DateTime{
      calendar: Calendar.ISO,
      day: 1,
      hour: Keyword.fetch!(opts, :hour),
      microsecond: {0, 0},
      minute: Keyword.fetch!(opts, :minute),
      month: 1,
      second: Keyword.fetch!(opts, :second),
      std_offset: 0,
      time_zone: "Etc/UTC",
      utc_offset: 0,
      year: 2020,
      zone_abbr: "UTC"
    }
  end
end
