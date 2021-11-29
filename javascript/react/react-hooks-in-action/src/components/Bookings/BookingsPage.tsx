import { useState } from "react";
import Bookable from "../../domain/Bookable";
import BookablesList from "../Bookables/BookablesList";
import Bookings from "./Bookings";

export default function BookingsPage() {
  const [bookable, setBookable] = useState<Bookable>();

  return (
    <main className="bookings-page">
      <BookablesList bookable={bookable} setBookable={setBookable} />
      <Bookings bookable={bookable} />
    </main>
  );
}
