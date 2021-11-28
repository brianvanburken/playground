export async function getData<T>(url: string): Promise<T> {
  const response = await fetch(url);
  if (!response.ok) {
    throw Error("There was a problem fetching data.");
  }
  return await response.json();
}
