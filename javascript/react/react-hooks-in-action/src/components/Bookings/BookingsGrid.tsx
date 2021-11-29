import { Dispatch, useEffect, useMemo, useState } from "react";
import { FaSpinner } from "react-icons/fa";
import Bookable from "../../domain/Bookable";
import Booking, { Session } from "../../domain/Booking";
import { getBookings } from "../../utils/api";
import { getGrid, Grid, GridSession, transformBookings } from "./grid-builder";
import { WeekState } from "./weekReducer";

export interface BookingsGridProps {
  week: WeekState;
  bookable?: Bookable;
  booking?: Booking;
  setBooking: Dispatch<Booking | undefined>;
}

export default function BookingsGrid({
  week,
  bookable,
  booking,
  setBooking,
}: BookingsGridProps) {
  const [bookings, setBookings] = useState<Grid>();
  const [error, setError] = useState();

  const { grid, sessions, dates } = useMemo(
    () =>
      bookable
        ? getGrid(bookable, week.start)
        : { grid: {} as Grid, sessions: [], dates: [] },
    [bookable, week.start]
  );

  useEffect(() => {
    if (!bookable) {
      return;
    }

    let doUpdate = true;
    setBookings(undefined);
    setError(undefined);
    setBooking(undefined);

    getBookings(bookable.id, week.start, week.end)
      .then((resp) => {
        if (doUpdate) {
          const grid = transformBookings(resp);
          setBookings(grid);
        }
      })
      .catch(setError);

    return () => {
      doUpdate = false;
    };
  }, [week, bookable, setBooking]);

  function cell(session: Session, date: string) {
    const cellData: Booking | undefined =
      (bookings?.[session] || ({} as GridSession))?.[date] ||
      (grid[session] || ({} as GridSession))?.[date];
    const isSelected = booking?.session === session && booking?.date === date;
    return (
      <td
        key={date}
        className={isSelected ? "selected" : undefined}
        onClick={bookings ? () => setBooking(cellData) : undefined}
      >
        {cellData?.title}
      </td>
    );
  }

  return (
    <>
      {error && (
        <p className="bookingsError">
          {`There was a problem loading the bookings data (${error})`}
        </p>
      )}

      <table className={bookings ? "bookingsGrid active" : "bookingsGrid"}>
        <thead>
          <tr>
            <th>
              <span className="status">
                <FaSpinner />
              </span>
            </th>
            {dates.map((d) => (
              <th key={d}>{new Date(d).toDateString()}</th>
            ))}
          </tr>
        </thead>
        <tbody>
          {sessions.map((session) => (
            <tr key={session}>
              <th>{session}</th>
              {dates.map((date) => cell(session, date))}
            </tr>
          ))}
        </tbody>
      </table>
    </>
  );
}
