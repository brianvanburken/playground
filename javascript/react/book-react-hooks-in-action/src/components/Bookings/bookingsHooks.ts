import { useMemo } from "react";
import { useQuery } from "react-query";
import { useSearchParams } from "react-router-dom";
import { isDate } from "util";
import Bookable from "../../domain/Bookable";
import Booking from "../../domain/Booking";
import { getData } from "../../utils/api";
import { shortISO } from "../../utils/date-wrangler";
import { getGrid, Grid, transformBookings } from "./grid-builder";

export function useBookings(
  bookableId: number | undefined,
  startDate: Date,
  endDate: Date
) {
  const start = shortISO(startDate);
  const end = shortISO(endDate);
  const urlRoot = "http://localhost:3001/bookings";
  const queryString = `bookableId=${bookableId}&date_gte=${start}&date_lte=${end}`;

  const query = useQuery<Booking[], Error>(
    ["bookings", bookableId, start, end],
    () => getData<Booking[]>(`${urlRoot}?${queryString}`)
  );

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

export function useBookingsParams() {
  const [searchParams, setSearchParams] = useSearchParams();
  const searchDate = searchParams.get("date");
  const bookableId = searchParams.get("bookableId");

  const date = isDate(searchDate) ? new Date(searchDate) : new Date();

  const idInt = parseInt(bookableId || "", 10);
  const hasId = !isNaN(idInt);

  function setBookingsDate(date: string) {
    const params: { bookableId?: string; date?: string } = {};

    if (hasId && bookableId) {
      params.bookableId = bookableId;
    }
    if (isDate(date)) {
      params.date = date;
    }

    if (!!params.date || !!params.bookableId) {
      setSearchParams(params, { replace: true });
    }
  }

  return {
    date,
    bookableId: hasId ? idInt : undefined,
    setBookingsDate,
  };
}
