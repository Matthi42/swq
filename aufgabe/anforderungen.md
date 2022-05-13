---
include-before: |
    # Dokumentation
    ## Inhaltsverzeichnis
---

## User Storys

1. Als Sachbearbeiter möchte ich einen Namen mit Anrede eingeben und dieser soll in seine Bestandteile zerlegt werden.

2. Als Sachbearbeiter möchte ich, dass Vor- und Nachname in verschiedener Reihenfolge eingegeben werden kann, damit die Bedienung vereinfacht ist.

3. Als Sachbearbeiter möchte ich, dass die Anrede, für Briefe oder E-Mails, korrekt formatiert ausgegeben wird.

4. Als Sachbearbeiter möchte ich auch die Kürzel der Anreden verwenden können.

5. Als Sachbearbeiter möchte ich, dass jederzeit die Bestandteile auch manuell bearbeitet werden können (insbesondere wenn diese nicht erkannt werden).

6. Als Sachbearbeiter möchte ich, dass ich neue Titel selber hinzufügen kann. Damit unbekannte Anreden in Zukunft unterstützt werden.


## Bewertung der Storys

| User Story | Storypoint | Akzeptanzkriterien                                                                                                           |
|------------|------------|------------------------------------------------------------------------------------------------------------------------------|
| 1          | 5          | Beim Absenden der Eingabe (vergleiche Testdaten) soll diese in ihre Bestandteile zerlegt werden und gespeichert werden.      |
| 2          | 2          | Es sollen sowohl "Anrede Vorname Nachname" und "Anrede Nachname, Vorname" erkannt werden.                                    |
| 3          | 1          | Nach dem Absenden sollen die Bestandteile in korrekter Form für Briefe angezeigt werden.                                     |
| 4          | 1          | Ausgeschriebene und abgekürzte Titel sollen möglich sein.                                                                    |
| 5          | 2          | Wenn das Geschlecht nicht erkannt werden kann, soll es manuell angegeben werden können.                                      |
| 6          | 3          | Neue Titel sollen hinzugefügt werden können. Danach soll die neue Anrede auch in der normalen Eingabe genutzt werden können. |

## Definiton of Done

- Die Akzeptanzkriterien der User Storys sind erfüllt.
- Alle Abhängigkeiten sind kontrolliert.
- Unit-Test Abdeckung der Business-Logic 75%
- Keine Kompilierfehler
- Dokumentation erstellt
- Alle erstellten Tests werden bestanden 

## Design 

### Annahmen

- Titel sind sprachen agnostisch, d.h. sie bleiben in allen Sprachen gleich 
- Sprache wird aus der Anrede / Geschlecht erkannt und kann noch angepasst werden

### Grobe Modellierung 
Es wurde sich für die Elm Architektur entschieden. Elm ist eine funktionale Architektur um Web-Pages zu erstellen.
Dabei wird der "Zustand" der App im Model gespeichert.
Die Darstellung wird mit einer Funktion erzielt, die das Model nimmt und eine HTML-Seite erstellt.
Um bei Events, wie einer Benutzereingabe, eine Änderung hervorzurufen wird eine Message verwendet.
Die Funktion **update**  die das alte Model und die Message nimmt gibt dann Seiteneffektfrei das neue Model zurück. 

![Elm Architektur](architecture-overview-diagram.svg)

Quelle: [The Elm Architecture](https://dennisreimann.de/articles/elm-architecture-overview.html)

Message-Typen:
``` haskell
data InsertMsg
  = Input String
  | GoEdit

data EditMsg
  = ChangeGeschlecht (Maybe Geschlecht)
  | ChangeVorname (Maybe String)
  | ChangeNachname String
  | ChangeSprache Sprache
  | ChangeTitel (Array Titel)
  | Save

data Msg
  = Insert InsertMsg
  | Edit EditMsg
```
Die Messages wurden in Änderungen und Einfügen eingeteilt.

##### Funktions Typ von update
``` haskell
update :: Model -> Msg -> Model
```
