import Bookable from "../../domain/Bookable";
import Booking, { Session } from "../../domain/Booking";
import { sessions as sessionNames } from "../../static.json";
import { addDays, shortISO } from "../../utils/date-wrangler";

export type Grid = Record<Session, GridSession>;

export interface GridSession {
  [key: string]: Booking;
}

export function getGrid(bookable: Bookable, startDate: Date) {
  const dates = bookable.days
    .sort()
    .map((d) => shortISO(addDays(startDate, d)));

  const sessions = bookable.sessions.map((i) => sessionNames[i] as Session);

  const grid = sessions.reduce(
    (grid: Partial<Grid>, session: Session): Partial<Grid> => {
      grid[session] = dates.reduce((gridSession: GridSession, date: string) => {
        gridSession[date] = {
          session,
          date,
          bookableId: bookable.id,
          title: "",
        };
        return gridSession;
      }, {});
      return grid;
    },
    {}
  ) as Grid;

  return {
    grid,
    dates,
    sessions,
  };
}

export function transformBookings(bookingsArray: Booking[]) {
  return bookingsArray.reduce((bookings: Partial<Grid>, booking: Booking) => {
    const { session, date } = booking;

    bookings[session] = bookings[session] ?? {};
    // We need to ignore the next line due to limitation of TypeScript
    //@ts-ignore
    bookings[session][date] = booking;

    return bookings;
  }, {}) as Grid;
}
