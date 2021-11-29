import { useReducer, useState } from "react";
import Bookable from "../../domain/Bookable";
import Booking from "../../domain/Booking";
import { getWeek } from "../../utils/date-wrangler";
import BookingDetails from "./BookingDetails";
import BookingsGrid from "./BookingsGrid";
import WeekPicker from "./WeekPicker";
import weekReducer from "./weekReducer";

export interface BookingsProps {
  bookable?: Bookable;
}

export default function Bookings({ bookable }: BookingsProps) {
  const [week, dispatch] = useReducer(weekReducer, new Date(), getWeek);
  const [booking, setBooking] = useState<Booking>();

  return (
    <div className="bookings">
      <div>
        <WeekPicker dispatch={dispatch} />
        <BookingsGrid
          week={week}
          bookable={bookable}
          booking={booking}
          setBooking={setBooking}
        />
      </div>
      <BookingDetails booking={booking} bookable={bookable} />
    </div>
  );
}
