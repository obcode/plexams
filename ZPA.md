# Schnittstelle zu ZPA

## Import vom ZPA

### Lister aller Personen

JSON-Array bestehend aus  Objekten der folgenden Form:

```json
{
    "is_lba": false,
    "email": "max.mustermann@hm.edu",
    "fk": "FK07",
    "person_shortname": "Mustermann, M.",
    "person_fullname": "Prof. Dr. Max Mustermann",
    "person_id": 15
}
```

### Initialer Plan

JSON-Array bestehend aus  Objekten der folgenden Form:

```json
{
    "duration": 90,
    "module": "Compiler",
    "main_examer": "Mustermann, M.",
    "is_repeater_exam": false,
    "main_examer_id": 180,
    "groups": [
        "IC",
        "IF4A"
    ],
    "anCode": 1111,
    "exam_type": "sp"
}
```

-   `main_examer_id`:  entspricht `person_id` aus der Liste aller Personen
-   `main_examer`:  entspricht `person_shortname`  aus der Liste aller Personen

### Anforderungen der Prüfungsausichten

JSON-Array bestehend aus  Objekten der folgenden Form:

```json
{
    "oral_exams_contribution": 0,
    "master_contribution": 0,
    "invigilator": "Mustermann, M.",
    "overtime_this_semester": 0.0,
    "part_time": 1.0,
    "free_semester": 0.0,
    "excluded_dates": [
        "13.07.17",
        "14.07.17",
        "17.07.17",
        "18.07.17"
    ],
    "inviligator_id": 15,
    "overtime_last_semester": 0.0
  },
```

-   `inviligator_id`:  entspricht `person_id` aus der Liste aller Personen
-   `invigilator`:  entspricht `person_shortname`  aus der Liste aller Personen
-   `excluded_dates`: Array der nicht einplanbaren Tage
-   `part_time`:
    -   `1.0` für Vollzeit
    -    entsprechender Anteil für Teilzeit, z.B. `0.6` für 60%
-   `free_semester`:
    -   `0.0` für kein Freisemester
    -   `0.5` für ein halbes Freisemester
    -   `1.0` für ein ganzes Freisemester
-   `overtime_last_semester`:
    -   `0.0` keinen Einfluss auf Berechnung
    -   `1.5` im Vorsemester 50% mehr Aufsichten gemacht
-   `overtime_this_semester`:
    -   `0.0` keinen Einfluss auf Berechnung
    -   `1.5` in diesem Semester 50% mehr Aufsichten planen
-   `oral_exams_contribution`:
    Anzahl Minuten als Beisitzer für mündliche Prüfungen
-   `master_contribution`:
    Anzahl Minuten für Auswahlgespräche für Master

## Export für das ZPA

JSON-Array, bestehend aus Objekten der folgenden Form:

```json
{
    "anCode": 101,
    "date": "17.07.2017",
    "time": "14:30",
    "total_number": 21,
    "reserveInvigilator_id": 123,
    "rooms": [
        {
            "number": "R0.012",
            "invigilator_id": 145,
            "numberStudents": 21,
            "reserveRoom": false,
            "handicapCompensation": false,
            "duration": 90
        }
    ]
}
```

`anCode`, `reserveInvigilator_id` und `invigilator_id` werden
durch das ZPA generiert und vorher in Plexams importiert.

Bedeutung der, nicht selbst erklärenden, Attribute:

-   `total_number`: Gesamtzahl der Anmeldungen
-   `numberStudents`: Anzahl der in dem Raum eingeplanten Prüflinge

    Die Summe aller `numberStudents` entspricht der `total_number`
    der Prüfung.


Der Plan muss vom ZPA in folgenden Versionen eingelesen werden können:

-   ohne Räume und ohne Aufsichten

    -   `"rooms" = []`
    -   `"reserveInvigilator_id" = 0`

-   mit Räumen aber ohne Aufsichten

    -   `"reserveInvigilator_id" = 0`
    -   `"invigilator_id": 0`


-   mit Räumen und Aufsichten
