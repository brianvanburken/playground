import { ChangeEvent, Dispatch, useEffect, useState } from "react";
import { FaArrowRight, FaSpinner } from "react-icons/fa";
import Bookable from "../../domain/Bookable";
import { getData } from "../../utils/api";

export interface BookablesListProps {
  bookable?: Bookable;
  setBookable: Dispatch<Bookable>;
}

export default function BookablesList({
  bookable,
  setBookable,
}: BookablesListProps) {
  const [bookables, setBookables] = useState<Bookable[]>([]);
  const [error, setError] = useState<Error>();
  const [isLoading, setIsLoading] = useState(true);

  const group = bookable?.group;
  const bookablesInGroup = bookables.filter((b) => b.group === group);
  const groups = Array.from(new Set(bookables.map((b) => b.group)));

  useEffect(() => {
    setIsLoading(true);

    getData<Bookable[]>("http://localhost:3001/bookables")
      .then((bookables) => {
        setBookable(bookables[0]);
        setBookables(bookables);
      })
      .catch(setError)
      .finally(() => setIsLoading(false));
  }, [setBookable]);

  if (error) {
    return <p>{error.message}</p>;
  }

  if (isLoading) {
    return (
      <p>
        <FaSpinner /> Loading bookables...
      </p>
    );
  }

  function changeGroup(event: ChangeEvent<HTMLSelectElement>) {
    const bookablesInSelectedGroup = bookables.filter(
      (b) => b.group === event.target.value
    );
    setBookable(bookablesInSelectedGroup[0]);
  }

  function changeBookable(selectedBookable: Bookable) {
    setBookable(selectedBookable);
  }

  function nextBookable() {
    const i = (bookable && bookablesInGroup.indexOf(bookable)) ?? 0;
    const nextIndex = (i + 1) % bookablesInGroup.length;
    const nextBookable = bookablesInGroup[nextIndex];
    setBookable(nextBookable);
  }

  return (
    <div>
      <select value={group} onChange={changeGroup}>
        {groups.map((g) => (
          <option value={g} key={g}>
            {g}
          </option>
        ))}
      </select>
      <ul className="bookables items-list-nav">
        {bookablesInGroup.map((b) => (
          <li
            key={b.id}
            className={b.id === bookable?.id ? "selected" : undefined}
          >
            <button className="btn" onClick={() => changeBookable(b)}>
              {b.title}
            </button>
          </li>
        ))}
      </ul>
      <p>
        <button className="btn" onClick={nextBookable} autoFocus>
          <FaArrowRight />
          <span>Next</span>
        </button>
      </p>
    </div>
  );
}
