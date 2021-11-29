import { Dispatch, useEffect, useState } from "react";
import { FaSpinner } from "react-icons/fa";
import User from "../../domain/User";
import { getData } from "../../utils/api";

export interface UsersListProps {
  user?: User;
  setUser: Dispatch<User>;
}

export default function UsersList({ user, setUser }: UsersListProps) {
  const [users, setUsers] = useState<User[]>([]);
  const [error, setError] = useState<Error>();
  const [isLoading, setIsLoading] = useState(true);

  useEffect(() => {
    setIsLoading(true);
    getData<User[]>("http://localhost:3001/users")
      .then((users) => {
        setUser(users[0]);
        setUsers(users);
      })
      .catch(setError)
      .finally(() => setIsLoading(false));
  }, [setUser]);

  if (error) {
    return <p>{error.message}</p>;
  }

  if (isLoading) {
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
