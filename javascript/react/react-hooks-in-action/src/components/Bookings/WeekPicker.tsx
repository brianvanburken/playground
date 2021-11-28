import { useReducer, useState } from "react";
import {
  FaCalendarCheck,
  FaCalendarDay,
  FaChevronLeft,
  FaChevronRight,
} from "react-icons/fa";
import { getWeek } from "../../utils/date-wrangler";
import reducer, { WeekActionType } from "./weekReducer";

export interface WeekPickerProps {
  date: Date;
}

export default function WeekPicker({ date }: WeekPickerProps) {
  const [week, dispatch] = useReducer(reducer, date, getWeek);
  const [dateText, setDateText] = useState("2020-06-24");

  function goToDate() {
    dispatch({
      type: WeekActionType.SetDate,
      payload: dateText
    });
  }

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

        <span>
          <input
            type="text"
            placeholder="e.g. 2020-09-02"
            value={dateText}
            onChange={(e) => setDateText(e.target.value)}
          />
          <button className="go btn" onClick={goToDate}>
            <FaCalendarCheck />
            <span>Go</span>
          </button>
        </span>

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
