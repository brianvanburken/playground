import { Dispatch, useEffect } from "react";
import { useQuery } from "react-query";
import User from "../../domain/User";
import { getData } from "../../utils/api";

export interface UsersListProps {
  user?: User;
  setUser: Dispatch<User>;
}

export default function UsersList({ user, setUser }: UsersListProps) {
  const { data: users = [] } = useQuery<User[], Error>(
    "users",
    () => getData<User[]>("http://localhost:3001/users"),
    { suspense: true }
  );

  useEffect(() => {
    setUser(users[0]);
  }, [users, setUser]);

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
