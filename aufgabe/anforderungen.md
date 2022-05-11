# Anforderungen

## User Storys

1. Als Sachbearbeiter möchte ich einen Namen mit Anrede eingeben und dieser soll in seine Bestandteile zerlegt werden.

2. Als Sachbearbeiter möchte ich, dass Vor- und Nachname in verschiedener Reihenfolge eingegeben werden kann, damit die Bedienung vereinfacht ist.

3. Als Sachbearbeiter möchte ich, dass die Anrede, für Briefe oder E-Mails, korrekt formatiert ausgegeben wird.

4. Als Sachbearbeiter möchte ich auch die Kürzel der Anreden verwenden können.

5. Als Sachbearbeiter möchte ich, dass jederzeit die Bestandteile auch manuell bearbeitet werden können (insbesondere wenn diese nicht erkannt werden).

6. Als Sachbearbeiter möchte ich, dass ich neue Titel selber hinzufügen kann. Damit unbekannte Anreden in Zukunft unterstützt werden.


## Bewertung der Storys

| User Story | Storypoint | Akzeptanzkriterien |
|------------|------------|--------------------|
| 1          | 5          | Beim Absenden der Eingabe (vergleiche Testdaten) soll diese in ihre Bestandteile zerlegt werden und gespeichert werden. | 
| 2 | 2 | Es sollen sowohl "Anrede Vorname Nachname" und "Anrede Nachname, Vorname" erkannt werden. |
| 3 | 1 | Nach dem Absenden sollen die Bestandteile in korrekter Form für Briefe angezeigt werden. |
| 4 | 1 | Ausgeschriebene und abgekürzte Titel sollen gleich behandelt werden. |
| 5 | 2 | Wenn das Geschlecht nicht erkannt werden kann, soll es manuell angegeben werden können. |
| 6 | 3 | Neue Anreden sollen mit Geschlecht und Briefform verknüpft werden können. Danach soll die neue Anrede auch in der normalen Eingabe genutzt werden können. |

## Design 
 Was genau soll gemacht werden??????????????????????????

### Annahmen

- Titel sind sprachenagnostisch, d.h. sie bleiben in allen Sprachen gleich 
- Sprache wird aus der Anrede / Geschlecht erkannt und kann noch angepasst werden

### Grobe Modellierung 


## Testfälle

## Fragen

Was passiert ohne Anrede?
