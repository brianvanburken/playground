import { useMemo } from "react";
import Bookable from "../../domain/Bookable";
import Booking from "../../domain/Booking";
import { shortISO } from "../../utils/date-wrangler";
import useFetch from "../../utils/useFetch";
import { getGrid, Grid, transformBookings } from "./grid-builder";

export function useBookings(
  bookableId: number | undefined,
  startDate: Date,
  endDate: Date
) {
  const start = shortISO(startDate);
  const end = shortISO(endDate);
  const urlRoot = "http://localhost:3001/bookings";
  const queryString =
    `bookableId=${bookableId}` + `&date_gte=${start}&date_lte=${end}`;

  const query = useFetch<Booking[]>(`${urlRoot}?${queryString}`);

  return {
    bookings: query.data ? transformBookings(query.data) : ({} as Grid),
    ...query,
  };
}

export function useGrid(bookable: Bookable | undefined, startDate: Date) {
  return useMemo(
    () =>
      bookable
        ? getGrid(bookable, startDate)
        : {
            grid: {} as Grid,
            sessions: [],
            dates: [],
          },
    [bookable, startDate]
  );
}
