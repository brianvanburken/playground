import { Dispatch, useEffect } from "react";
import { FaSpinner } from "react-icons/fa";
import { useQuery } from "react-query";
import User from "../../domain/User";
import { getData } from "../../utils/api";

export interface UsersListProps {
  user?: User;
  setUser: Dispatch<User>;
}

export default function UsersList({ user, setUser }: UsersListProps) {
  const {
    data: users = [],
    status,
    error,
  } = useQuery<User[], Error>("users", () =>
    getData<User[]>("http://localhost:3001/users")
  );

  useEffect(() => {
    setUser(users[0]);
  }, [users, setUser]);

  if (error && status === "error") {
    return <p>{error.message}</p>;
  }

  if (status === "loading") {
    return (
      <p>
        <FaSpinner /> Loading users...
      </p>
    );
  }

  return (
    <ul className="users items-list-nav">
      {users.map((u) => (
        <li key={u.id} className={u.id === user?.id ? "selected" : undefined}>
          <button className="btn" onClick={() => setUser(u)}>
            {u.name}
          </button>
        </li>
      ))}
    </ul>
  );
}
