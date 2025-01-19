# Cat Haven

This was just a tiny project I did to learn about the practical usage of Haskell. The backend is written entirely in Haskell, for the frontend I ran out of patience and just did a quick and dirty HTML/CSS/JS one page solution. I know Haskell has some libraries for frontend development, so perhaps I can look at that sometime later. But we all know how these things go, right?

I really wanted to have a pretty frontend with pictures, so I used the [Cat API](https://thecatapi.com/) to get some easy cat images. The backend is a simple REST API that manages a database of cats and comments. The frontend is a single page that shows all of the saved cats, allows you to add new cats, and add comments to existing cats.

## Tech stack

- Backend
    - Written in **Haskell**.
    - **Beam** for database management (SQLite).
    - **Scotty** for API routing and handling.
- Frontend
    - Written in **HTML/CSS/JS**.
    - **Bootstrap** for styling.

## Running the project

1. Make sure you have Haskell installed.
    - If not, you can install it using [GHCup](https://www.haskell.org/ghcup/), with the instructions on their website.
2. Clone the repository.
3. Run the Backend:
    - Navigate to the `backend` directory.
    - Run the backend using Stack:
        ```bash
        stack run
        ```
4. Run the Frontend:
    - Navigate to the `frontend` directory.
    - Open the `index.html` file in your browser.