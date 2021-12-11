import AddIcon from "@mui/icons-material/Add";
import { Button, Container, Divider, List } from "@mui/material";
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
    [setPersonen, setShowForm, editingPersoon, personen]
  );

  const deletePersoon = useCallback(
    (natuurlijkPersoon: NatuurlijkPersoonModel) => {
      setPersonen(personen.filter((p) => isSamePersoon(p, natuurlijkPersoon)));
    },
    [setPersonen, personen]
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
    <Container>
      <List>
        {personen
          .filter((p) => !isSamePersoon(p, editingPersoon))
          .map((p, i, all) => (
            <>
              <NatuurlijkPersoonListItem
                key={p.id}
                persoon={p}
                deletePersoon={deletePersoon}
                editPersoon={editPersoon}
              />
              {i < all.length - 1 && all.length > 1 ? (
                <Divider variant="inset" component="li" />
              ) : null}
            </>
          ))}
      </List>
      {isFormShown && (
        <NatuurlijkPersoonForm
          editingPersoon={editingPersoon}
          addPersoon={addPersoon}
          hideForm={hideForm}
        />
      )}
      {!isFormShown && (
        <Button
          onClick={() => setShowForm(true)}
          variant="contained"
          endIcon={<AddIcon />}
        >
          Persoon toevoegen
        </Button>
      )}
    </Container>
  );
}

function isSamePersoon(a: NatuurlijkPersoonModel, b?: NatuurlijkPersoonModel) {
  return a.id === b?.id;
}
