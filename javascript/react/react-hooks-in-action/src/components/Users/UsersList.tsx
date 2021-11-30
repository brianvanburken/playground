import { Dispatch, useEffect } from "react";
import { FaSpinner } from "react-icons/fa";
import User from "../../domain/User";
import useFetch from "../../utils/useFetch";

export interface UsersListProps {
  user?: User;
  setUser: Dispatch<User>;
}

export default function UsersList({ user, setUser }: UsersListProps) {
  const {
    data: users = [],
    status,
    error,
  } = useFetch<User[]>("http://localhost:3001/users");

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
