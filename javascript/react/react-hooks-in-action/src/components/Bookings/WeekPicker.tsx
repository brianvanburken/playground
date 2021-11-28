import { useReducer } from "react";
import { FaCalendarDay, FaChevronLeft, FaChevronRight } from "react-icons/fa";
import { getWeek } from "../../utils/date-wrangler";
import reducer, { WeekActionType } from "./weekReducer";

export interface WeekPickerProps {
  date: Date;
}

export default function WeekPicker({ date }: WeekPickerProps) {
  const [week, dispatch] = useReducer(reducer, date, getWeek);
  return (
    <div>
      <p className="date-picker">
        <button
          className="btn"
          onClick={() => dispatch({ type: WeekActionType.PrevWeek })}
        >
          <FaChevronLeft />
          <span>Prev</span>
        </button>
        <button
          className="btn"
          onClick={() => dispatch({ type: WeekActionType.Today })}
        >
          <FaCalendarDay />
          <span>Today</span>
        </button>
        <button
          className="btn"
          onClick={() => dispatch({ type: WeekActionType.NextWeek })}
        >
          <span>Next</span>
          <FaChevronRight />
        </button>
      </p>
      <p>
        {week.start.toDateString()} - {week.end.toDateString()}
      </p>
    </div>
  );
}
