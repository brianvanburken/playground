import { useCallback, useState } from "react";
import NatuurlijkPersoonForm from "./NatuurlijkPersoonForm";
import NatuurlijkPersoonListItem from "./NatuurlijkPersoonListItem";
import { NatuurlijkPersoonModel } from "./NatuurlijkPersoonModel";

export default function App() {
  const [personen, setPersonen] = useState<NatuurlijkPersoonModel[]>([]);
  const [isFormShown, setShowForm] = useState(false);
  const [editingPersoon, setEditPersoon] = useState<NatuurlijkPersoonModel>();

  const addPersoon = useCallback(
    (natuurlijkPersoon: NatuurlijkPersoonModel) => {
      const newPersonen = editingPersoon
        ? personen.map((p) =>
            isSamePersoon(p, editingPersoon) ? natuurlijkPersoon : p
          )
        : [...personen, natuurlijkPersoon];

      setPersonen(newPersonen);
      setEditPersoon(undefined);
      setShowForm(false);
    },
    [setPersonen, setShowForm]
  );

  const deletePersoon = useCallback(
    (natuurlijkPersoon: NatuurlijkPersoonModel) => {
      setPersonen(personen.filter((p) => isSamePersoon(p, natuurlijkPersoon)));
    },
    [setPersonen, setShowForm]
  );

  const editPersoon = useCallback(
    (natuurlijkPersoon: NatuurlijkPersoonModel) => {
      setEditPersoon(natuurlijkPersoon);
      setShowForm(true);
    },
    [setEditPersoon, setShowForm]
  );

  const hideForm = useCallback(() => {
    setEditPersoon(undefined);
    setShowForm(false);
  }, [setShowForm]);

  return (
    <>
      <div>
        {personen
          .filter((p) => !isSamePersoon(p, editingPersoon))
          .map((p) => (
            <NatuurlijkPersoonListItem
              persoon={p}
              deletePersoon={deletePersoon}
              editPersoon={editPersoon}
            />
          ))}
      </div>
      {isFormShown && (
        <NatuurlijkPersoonForm
          editingPersoon={editingPersoon}
          addPersoon={addPersoon}
          hideForm={hideForm}
        />
      )}
      {!isFormShown && (
        <button onClick={() => setShowForm(true)}>Persoon toevoegen</button>
      )}{" "}
    </>
  );
}

function isSamePersoon(a: NatuurlijkPersoonModel, b?: NatuurlijkPersoonModel) {
  return a.id === b?.id;
}
