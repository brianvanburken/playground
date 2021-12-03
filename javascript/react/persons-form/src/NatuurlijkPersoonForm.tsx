import { useForm } from "react-hook-form";
import "./NatuurlijkPersoonForm.css";
import { NatuurlijkPersoonModel } from "./NatuurlijkPersoonModel";

const geboortegemeentes = ["rhenen", "veenendaal", "amerongen"];
const voorvoegsels = ["van", "der", "van der"];

interface NatuurlijkPersoonFormProps {
  editingPersoon?: NatuurlijkPersoonModel;
  addPersoon: (_: NatuurlijkPersoonModel) => void;
  hideForm: () => void;
}

export default function NatuurlijkPersoonForm({
  editingPersoon,
  addPersoon,
  hideForm,
}: NatuurlijkPersoonFormProps) {
  const {
    clearErrors,
    register,
    handleSubmit,
    getValues,
    formState: { errors },
  } = useForm<NatuurlijkPersoonModel>({
    defaultValues: {
      id: new Date().getTime(),
      bsn: "",
      geboortedatum: "",
      geboortegemeente: "",
      geborenInBuitenland: false,
      geslachtsnaam: "",
      land: "",
      voornamen: "",
      voorvoegselGeslachtsnaam: "",
      woonplaats: "",
      ...(editingPersoon || {}),
    },
  });

  return (
    <form onSubmit={handleSubmit(addPersoon)}>
      <label>
        BSN
        <input
          {...register("bsn", {
            minLength: 8,
            maxLength: 9,
            validate: {
              elfproef: ifFilledIn(elfproef),
            },
          })}
        />
        {errors.bsn?.type === "pattern" && (
          <span className="error">BSN mag alleen cijfers bevatten.</span>
        )}
        {errors.bsn?.type === "minLength" && (
          <span className="error">BSN moet 8 of 9 cijfers lang zijn.</span>
        )}
        {errors.bsn?.type === "maxLength" && (
          <span className="error">BSN moet 8 of 9 cijfers lang zijn.</span>
        )}
        {errors.bsn?.type === "elfproef" && (
          <span className="error">BSN moet voldoen aan de elfproef.</span>
        )}
      </label>

      <label>
        Voornamen
        <input {...register("voornamen", { required: true, maxLength: 200 })} />
        {errors.voornamen?.type === "required" && (
          <span className="error">Voornamen is vereist.</span>
        )}
        {errors.voornamen?.type === "maxLength" && (
          <span className="error">
            Voornamen mag maximaal 200 karakters bevatten.
          </span>
        )}
      </label>

      <label>
        Voorvoegsels
        <input
          {...register("voorvoegselGeslachtsnaam", {
            maxLength: 10,
            validate: {
              included: ifFilledIn(isIncluded(voorvoegsels)),
            },
          })}
        />
        {errors.voorvoegselGeslachtsnaam?.type === "included" && (
          <span className="error">
            Voorvoegsel komt niet voor in lijst met bekende voorvoegsels.
          </span>
        )}
        {errors.voorvoegselGeslachtsnaam?.type === "maxLength" && (
          <span className="error">
            Voorvoegsels mag maximaal 10 karakters bevatten.
          </span>
        )}
      </label>

      <label>
        Geslachtsnaam
        <input
          {...register("geslachtsnaam", { required: true, maxLength: 200 })}
        />
        {errors.geslachtsnaam?.type === "required" && (
          <span className="error">Geslachtsnaam is vereist.</span>
        )}
        {errors.geslachtsnaam?.type === "maxLength" && (
          <span className="error">
            Geslachtsnaam mag maximaal 200 karakters bevatten.
          </span>
        )}
      </label>

      <label>
        Woonplaats
        <input {...register("woonplaats", { required: true, maxLength: 50 })} />
        {errors.woonplaats?.type === "required" && (
          <span className="error">Woonplaats is vereist.</span>
        )}
        {errors.woonplaats?.type === "maxLength" && (
          <span className="error">
            Woonplaats mag maximaal 50 karakters bevatten.
          </span>
        )}
      </label>

      <label>
        Land
        <select {...register("land", { required: true })}>
          <option value=""></option>
          <option value="NL">Nederland</option>
          <option value="BE">Belgie</option>
          <option value="DE">Duitsland</option>
        </select>
        {errors.land?.type === "required" && (
          <span className="error">Land is vereist.</span>
        )}
      </label>

      <label>
        Geboortegemeente
        <input
          {...register("geboortegemeente", {
            required: !getValues("geborenInBuitenland"),
            maxLength: 240,
            validate: {
              included: (v: string | undefined) => {
                console.log(
                  v,
                  !getValues("geborenInBuitenland"),
                  isIncluded(geboortegemeentes)(v ?? "")
                );
                return (
                  !getValues("geborenInBuitenland") &&
                  isIncluded(geboortegemeentes)(v ?? "")
                );
              },
            },
          })}
        />
        {errors.geboortegemeente?.type === "required" && (
          <span className="error">Geboortegemeente is vereist.</span>
        )}
        {errors.geboortegemeente?.type === "included" && (
          <span className="error">
            Geboortegemeente komt niet voor in lijst met bekende gemeentes.
          </span>
        )}
        {errors.geboortegemeente?.type === "maxLength" && (
          <span className="error">
            Geboortegemeente mag maximaal 240 karakters bevatten.
          </span>
        )}
      </label>

      <label>
        Geboortedatum
        <input
          placeholder="dd-mm-jjjj"
          {...register("geboortedatum", {
            required: true,
            pattern: /^\d{2}-\d{2}-\d{4}$/,
            validate: {
              future: ifFilledIn(dateInFuture),
              olderThan16: ifFilledIn(datePast16YearsAgo),
            },
          })}
        />
        {errors.geboortedatum?.type === "required" && (
          <span className="error">Geboortedatum is vereist.</span>
        )}
        {errors.geboortedatum?.type === "pattern" && (
          <span className="error">
            Geboortedatum moet volgens het formaat "dd-mm-jjjj" zijn opgemaakt.
          </span>
        )}
        {errors.geboortedatum?.type === "future" && (
          <span className="error">
            Geboortedatum mag niet in de toekomst liggen.
          </span>
        )}
        {errors.geboortedatum?.type === "olderThan16" && (
          <span className="error">
            Personen jonger dan 16 jaar kunnen niet geregistreerd worden.
          </span>
        )}
      </label>

      <label>
        Geboren in buitenland?
        <input
          {...register("geborenInBuitenland")}
          type="checkbox"
          onChange={() => clearErrors("geboortegemeente")}
        />
      </label>

      <button onClick={() => hideForm()}>Annuleren</button>
      <button type="submit">Opslaan</button>
    </form>
  );
}

function elfproef(bsn: string): boolean {
  bsn = bsn.padStart(9);
  let sum = 0;
  for (let i = 0, l = bsn.length; i < l; i++) {
    sum += parseInt(bsn[i], 10) * (i === l - 1 ? -1 : l - i);
  }
  return sum % 11 === 0;
}

function reverseDateFormat(d: string) {
  return d.split("-").reverse().join("-");
}

function dateInFuture(d: string) {
  return new Date(reverseDateFormat(d)) < new Date();
}

function datePast16YearsAgo(d: string) {
  const reversedDate = reverseDateFormat(d);
  const inputDate = new Date(reversedDate);
  const date = new Date();
  date.setFullYear(date.getFullYear() - 16);
  return inputDate <= date;
}

function ifFilledIn(f: (_: string) => boolean) {
  return (v: string | undefined) => !v || f(v);
}

function isIncluded(list: string[]) {
  return (v: string) => list.includes(v.trim().toLowerCase());
}
