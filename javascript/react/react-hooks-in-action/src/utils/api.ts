import Booking from "../domain/Booking";
import { shortISO } from "./date-wrangler";

export async function getData<T>(url: string): Promise<T> {
  const response = await fetch(url);
  if (!response.ok) {
    throw Error("There was a problem fetching data.");
  }
  return await response.json();
}

export async function getBookings(
  bookableId: number,
  startDate: Date,
  endDate: Date
) {
  const start = shortISO(startDate);
  const end = shortISO(endDate);

  const urlRoot = "http://localhost:3001/bookings";

  const query = `bookableId=${bookableId}&date_gte=${start}&date_lte=${end}`;

  return getData<Booking[]>(`${urlRoot}?${query}`);
}
