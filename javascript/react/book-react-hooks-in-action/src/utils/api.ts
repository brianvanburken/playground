import Booking from "../domain/Booking";
import { shortISO } from "./date-wrangler";

export async function getData<T>(url: string): Promise<T> {
  const response = await fetch(url);
  if (!response.ok) {
    throw Error("There was a problem fetching data.");
  }
  return response.json();
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

export async function createItem<T>(url: string, item: T): Promise<T> {
  const response = await fetch(url, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(item),
  });
  if (!response.ok) {
    throw new Error("There was a problem creating the item!");
  }
  return response.json();
}

export async function editItem<T>(url: string, item: T): Promise<T> {
  const response = await fetch(url, {
    method: "PUT",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(item),
  });
  if (!response.ok) {
    throw new Error("There was a problem updating the item!");
  }
  return response.json();
}

export async function deleteItem<T>(url: string): Promise<T> {
  const response = await fetch(url, {
    method: "DELETE",
    headers: { "Content-Type": "application/json" },
  });
  if (!response.ok) {
    throw new Error("There was a problem deleting the item!");
  }
  return response.json();
}
