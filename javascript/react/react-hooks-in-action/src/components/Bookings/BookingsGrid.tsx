import { Dispatch, useEffect } from "react";
import { FaSpinner } from "react-icons/fa";
import Bookable from "../../domain/Bookable";
import Booking, { Session } from "../../domain/Booking";
import { useBookings, useGrid } from "./bookingsHooks";
import { GridSession } from "./grid-builder";
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
  const { bookings, status, error } = useBookings(
    bookable?.id,
    week.start,
    week.end
  );

  const { grid, sessions, dates } = useGrid(bookable, week.start);

  useEffect(() => {
    setBooking(undefined);
  }, [bookable, week.start, setBooking]);

  function cell(session: Session, date: string) {
    const cellData: Booking | undefined =
      (bookings?.[session] || ({} as GridSession))?.[date] ||
      (grid[session] || ({} as GridSession))?.[date];
    const isSelected = booking?.session === session && booking?.date === date;
    return (
      <td
        key={date}
        className={isSelected ? "selected" : undefined}
        onClick={status === "success" ? () => setBooking(cellData) : undefined}
      >
        {cellData?.title}
      </td>
    );
  }

  if (!grid) {
    return <p>Waiting for bookable and week details...</p>;
  }

  return (
    <>
      {status === "error" && (
        <p className="bookingsError">
          {`There was a problem loading the bookings data (${error})`}
        </p>
      )}

      <table
        className={
          status === "success" ? "bookingsGrid active" : "bookingsGrid"
        }
      >
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
