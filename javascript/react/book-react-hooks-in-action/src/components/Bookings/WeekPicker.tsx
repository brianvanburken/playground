import { RefObject, useRef } from "react";
import {
  FaCalendarCheck,
  FaCalendarDay,
  FaChevronLeft,
  FaChevronRight,
} from "react-icons/fa";
import { addDays, shortISO } from "../../utils/date-wrangler";
import { useBookingsParams } from "./bookingsHooks";

export default function WeekPicker() {
  const textboxRef = useRef<HTMLInputElement>();

  const { date, setBookingsDate: goToDate } = useBookingsParams();

  const dates = {
    prev: shortISO(addDays(date, -7)),
    next: shortISO(addDays(date, 7)),
    today: shortISO(new Date()),
  };

  return (
    <div>
      <p className="date-picker">
        <button className="btn" onClick={() => goToDate(dates.prev)}>
          <FaChevronLeft />
          <span>Prev</span>
        </button>
        <button className="btn" onClick={() => goToDate(dates.today)}>
          <FaCalendarDay />
          <span>Today</span>
        </button>

        <span>
          <input
            type="text"
            placeholder="e.g. 2020-09-02"
            defaultValue="2020-06-24"
            ref={textboxRef as RefObject<HTMLInputElement>}
          />
          <button
            className="go btn"
            onClick={() => goToDate(textboxRef?.current?.value ?? "")}
          >
            <FaCalendarCheck />
            <span>Go</span>
          </button>
        </span>

        <button className="btn" onClick={() => dates.next}>
          <span>Next</span>
          <FaChevronRight />
        </button>
      </p>
    </div>
  );
}
