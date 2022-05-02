import { createContext, Dispatch, useContext, useState } from "react";
import User from "../../domain/User";

export interface UserContextValues {
  user?: User;
  setUser: Dispatch<User | undefined>;
}

const UserContext = createContext<User | undefined>(undefined);

const UserSetContext = createContext<Dispatch<User | undefined>>(
  () => undefined
);

export interface UserProviderProps {
  children?: React.ReactNode;
}

export function UserProvider({ children }: UserProviderProps) {
  const [user, setUser] = useState<User | undefined>();

  return (
    <UserContext.Provider value={user}>
      <UserSetContext.Provider value={setUser}>
        {children}
      </UserSetContext.Provider>
    </UserContext.Provider>
  );
}

export type UseUserReturnValue = [
  user: User | undefined,
  setUser: Dispatch<User | undefined>
];

export function useUser(): UseUserReturnValue {
  const user = useContext(UserContext);
  const setUser = useContext(UserSetContext);

  if (!setUser) {
    throw new Error("The UserProvider is missing.");
  }

  return [user, setUser];
}
