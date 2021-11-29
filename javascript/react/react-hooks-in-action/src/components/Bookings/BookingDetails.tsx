import Bookable from "../../domain/Bookable";
import Booking from "../../domain/Booking";

export interface BookingDetailsProps {
  booking?: Booking;
  bookable?: Bookable;
}

export default function BookingDetails(_: BookingDetailsProps) {
  return (
    <div className="booking-details placeholder">
      <h3>Booking Details</h3>
    </div>
  );
}
