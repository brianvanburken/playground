import { useState } from "react";
import Bookable from "../../domain/Bookable";
import BookableDetails from "./BookableDetails";
import BookablesList from "./BookablesList";

export default function BookablesView() {
  const [bookable, setBookable] = useState<Bookable>();

  return (
    <>
      <BookablesList bookable={bookable} setBookable={setBookable} />
      <BookableDetails bookable={bookable} />
    </>
  );
}
