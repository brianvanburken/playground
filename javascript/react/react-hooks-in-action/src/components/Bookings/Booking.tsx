import Bookable from "../../domain/Bookable";
import BookingModel from "../../domain/Booking";

export interface BookingProps {
  booking: BookingModel;
  bookable?: Bookable;
}

export default function Booking({ booking, bookable }: BookingProps) {
  const { title, date, session, notes } = booking;

  return (
    <div className="booking-details-fields">
      <label>Title</label>
      <p>{title}</p>

      <label>Bookable</label>
      <p>{bookable?.title}</p>

      <label>Booking Date</label>
      <p>{new Date(date).toDateString()}</p>

      <label>Session</label>
      <p>{session}</p>

      {notes && (
        <>
          <label>Notes</label>
          <p>{notes}</p>
        </>
      )}
    </div>
  );
}
