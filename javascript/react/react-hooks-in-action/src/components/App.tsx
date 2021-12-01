import { lazy, Suspense } from "react";
import { FaCalendarAlt, FaDoorOpen, FaSpinner, FaUsers } from "react-icons/fa";
import { QueryClient, QueryClientProvider } from "react-query";
import { BrowserRouter as Router, Link, Route, Routes } from "react-router-dom";
import "../App.css";
import ErrorBoundary from "./UI/ErrorBoundary";
import { UserProvider } from "./Users/UserContext";
import UserPicker from "./Users/UserPicker";

export default function App() {
  const queryClient = new QueryClient();

  const BookablesPage = lazy(() => import("./Bookables/BookablesPage"));
  const BookingsPage = lazy(() => import("./Bookings/BookingsPage"));
  const UsersPage = lazy(() => import("./Users/UsersPage"));

  return (
    <QueryClientProvider client={queryClient}>
      <UserProvider>
        <Router>
          <div className="App">
            <header>
              <nav>
                <ul>
                  <li>
                    <Link to="/bookings" className="btn btn-header">
                      <FaCalendarAlt />
                      <span>Bookings</span>
                    </Link>
                  </li>
                  <li>
                    <Link to="/bookables" className="btn btn-header">
                      <FaDoorOpen />
                      <span>Bookables</span>
                    </Link>
                  </li>
                  <li>
                    <Link to="/users" className="btn btn-header">
                      <FaUsers />
                      <span>Users</span>
                    </Link>
                  </li>
                </ul>
              </nav>
              <UserPicker />
            </header>
            <ErrorBoundary
              fallback={
                <>
                  <h1>Something went wrong!</h1>
                  <p>Try reloading the page.</p>
                </>
              }
            >
              <Suspense
                fallback={
                  <>
                    <FaSpinner /> Loading page and data...
                  </>
                }
              >
                <Routes>
                  <Route path="/bookings" element={<BookingsPage />} />
                  <Route path="/bookables/*" element={<BookablesPage />} />
                  <Route path="/users" element={<UsersPage />} />
                </Routes>
              </Suspense>
            </ErrorBoundary>
          </div>
        </Router>
      </UserProvider>
    </QueryClientProvider>
  );
}
