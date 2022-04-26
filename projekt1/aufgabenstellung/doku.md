# User stories

## 1. Korrekte Eingabe | 3
Als Benutzer möchte ich, dass meine Eingabe immer korrekt aufgeteilt wird,
damit der Kunde kontaktiert werden kann.

## 2. Tolerante Eingabe | 3
Als Benutzer möchte ich, dass meine Eingabe nicht der ISO-Norm entsprechen
muss, um angenommen zu werden (Leerzeichen ignorieren), damit die Benutzung
einfach ist.

## 3. Fehlerrückmeldung | 1
Als Benutzer möchte ich eine angemessene Fehlerrückmeldung, wenn die Eingabe
falsch ist, um schnell Fehler zu bemerken und korrigieren.

## 4. Ausgabe | 3
Als Benutzer möchte ich eine übersichtliche, moderne und gleichförmige Ausgabe
für jede korrekte Nummer, damit ich schnell alle Elemente erkennen kann.

## 5. Ausgabe | 1
Als Benutzer möchte ich, dass die Länder als 2-Stelliges Kürzel nach dem
dazugehörigen ISO-Standard angezeigt werden, um diese einfach identifizieren zu
können.


# Architektur

![Grobe Architektur](./imgs/architektur.png)


# Detaillierte Planung

```haskell
main :: Request -> Html
-- main = do 
--     allNumbers <- newIORef []
--     (processRequest allNumbers) =<< parseRequest

-- GET url.example.com/ --> HTML
-- POST url.example.com/ --> parse -> HTML mit Number

data NumberRequest = GetHTML 
                   | PostNumber String
                   | WrongEndpoint

parseRequest :: Request -> NumberRequest
processRequest :: IORef [Number] -> NumberRequest -> IO Html 

renderForm :: Html 
render404 :: Html

parseAndRender :: IORef [Number] -> String -> IO Html
parseNumber :: String -> Either NumberError Number

renderError :: NumberError -> Html 
renderResult :: Number -> [Number] -> Html 

data NumberError = IllegalChars String 
                 | IncorrectLength Int
                 | UnknownCountryCode String

type LandISOCode = String
data Number = Number
    { countryCode :: LandISOCode
    , areaCode    :: String
    , mainNumber  :: String
    , extension   :: Maybe String
    }
```

# Backlog 

1. 1 Johannes
2. 4 Matthi
3. 5
4. 3
5. 2

## Klausur

- Instant Feedback
- Doku
- Release Note 
- Fehlerdiagnose
- User Stories
- Test (pro User Story)
