import { FaPlus } from "react-icons/fa";
import { useQuery } from "react-query";
import { Link, useParams } from "react-router-dom";
import Bookable from "../../domain/Bookable";
import { getData } from "../../utils/api";
import BookableDetails from "./BookableDetails";
import BookablesList from "./BookablesList";

export default function BookablesView() {
  const { data: bookables = [] } = useQuery<Bookable[], Error>(
    "bookables",
    () => getData<Bookable[]>("http://localhost:3001/bookables"),
    { suspense: true }
  );

  const { id } = useParams();

  const bookable =
    (!!id && bookables.find((b) => b.id === parseInt(id, 10))) || bookables[0];

  return (
    <main className="bookables-page">
      <div>
        <BookablesList
          bookable={bookable}
          bookables={bookables}
          getUrl={(id: number) => `/bookables/${id}`}
        />
        <p className="controls">
          <Link to="/bookables/new" replace={true} className="btn">
            <FaPlus />
            <span>New</span>
          </Link>
        </p>
      </div>
      <BookableDetails bookable={bookable} />
    </main>
  );
}
