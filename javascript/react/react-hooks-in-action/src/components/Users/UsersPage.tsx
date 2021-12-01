import { Suspense, useState } from "react";
import { FaSpinner } from "react-icons/fa";
import { useQueryClient } from "react-query";
import User from "../../domain/User";
import { getData } from "../../utils/api";
import { useUser } from "./UserContext";
import UserDetails from "./UserDetails";
import UsersList from "./UsersList";

export default function UsersPage() {
  const [loggedInUser] = useUser();
  const [selectedUser, setSelectedUser] = useState<User>();
  const user = selectedUser || loggedInUser;
  const queryClient = useQueryClient();

  function switchUser(nextUser: User) {
    setSelectedUser(nextUser);

    queryClient.prefetchQuery(["user", nextUser.id], () =>
      getData(`http://localhost:3001/users/${nextUser.id}`)
    );

    queryClient.prefetchQuery(
      `http://localhost:3001/img/${nextUser.img}`,
      () =>
        new Promise((resolve) => {
          const img = new Image();
          img.onload = () => resolve(img);
          img.src = `http://localhost:3001/img/${nextUser.img}`;
        })
    );
  }

  return user ? (
    <main className="users-page">
      <UsersList user={user} setUser={switchUser} />
      <Suspense fallback={<FaSpinner />}>
        <UserDetails userId={user?.id ?? 0} />
      </Suspense>
    </main>
  ) : null;
}
