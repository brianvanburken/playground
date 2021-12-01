import { useEffect, useState } from "react";
import Bookable from "../../domain/Bookable";
import Booking from "../../domain/Booking";
import { getWeek, shortISO } from "../../utils/date-wrangler";
import BookingDetails from "./BookingDetails";
import BookingsGrid from "./BookingsGrid";
import { useBookings, useBookingsParams } from "./bookingsHooks";
import { GridSession } from "./grid-builder";
import WeekPicker from "./WeekPicker";

export interface BookingsProps {
  bookable?: Bookable;
}

export default function Bookings({ bookable }: BookingsProps) {
  const [booking, setBooking] = useState<Booking>();
  const { date } = useBookingsParams();
  const week = getWeek(date);
  const weekStart = shortISO(week.start);
  const { bookings } = useBookings(bookable?.id, week.start, week.end);

  const sessionGrid: GridSession | undefined =
    booking && bookings && bookings[booking.session];
  const selectedBooking = sessionGrid && booking && sessionGrid[booking.date];

  useEffect(() => {
    setBooking(undefined);
  }, [bookable, weekStart]);

  return (
    <div className="bookings">
      <div>
        <WeekPicker />
        <BookingsGrid
          week={week}
          bookable={bookable}
          booking={booking}
          setBooking={setBooking}
        />
      </div>
      <BookingDetails
        booking={selectedBooking || booking}
        bookable={bookable}
      />
    </div>
  );
}
