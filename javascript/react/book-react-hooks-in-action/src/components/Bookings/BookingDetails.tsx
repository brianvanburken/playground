import { FaEdit } from "react-icons/fa";
import Bookable from "../../domain/Bookable";
import BookingModel from "../../domain/Booking";
import { useUser } from "../Users/UserContext";
import Booking from "./Booking";

export interface BookingDetailsProps {
  booking?: BookingModel;
  bookable?: Bookable;
}

export default function BookingDetails({
  booking,
  bookable,
}: BookingDetailsProps) {
  const [user] = useUser();
  const isBooker = booking && user && booking.bookerId === user.id;

  return (
    <div className="booking-details">
      <h2>
        Booking Details
        {isBooker && (
          <span className="controls">
            <button className="btn">
              <FaEdit />
            </button>
          </span>
        )}
      </h2>
      {booking ? (
        <Booking booking={booking} bookable={bookable} />
      ) : (
        <div className="booking-details-fields">
          <p>Select a booking or a booking slot.</p>
        </div>
      )}
    </div>
  );
}
