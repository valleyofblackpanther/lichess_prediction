# Lichess Data Application

## Overview
This application interfaces with the LichessAPI to retrieve data from the lichess.org platform's database. It provides functionalities to manage player data, perform chess match predictions using a machine learning model, and interact with a local database to store retrieved data.

## Features
- Retrieve player information, including url, games played, wins, draws, and losses, and display ratings for common chess time controls.
- Add, delete, and retrieve player data in the database.
- Utilize a machine learning model to predict the outcome of chess matches.
- Export the database to a JSON file.

## Database Design
The database consists of three tables: `players`, `gametypes`, and `games`, which store player data, game type information, and individual game data, respectively. This design facilitates easy calculation of averages and rating differentials.

## Running the Application
Execute the application with `stack run`, followed by the desired commands and arguments:

- `getPlayerInfo [p]`: Retrieve a player's information from the database.
- `addPlayer [p]`: Add a new player to the database.
- `deletePlayer [p]`: Delete a player from the database.
- `initialiseModel`: Initialize the machine learning model.
- `predict [p1, p2]`: Predict the winner between two players in the database.
- `export`: Write the database to a file in JSON format.

## Extra Challenging Feature
The application includes an advanced feature that allows users to initialize and train a machine learning model. The model, based on logistic regression, predicts the outcome of matches by analyzing the rating difference between two players. The process involves feature engineering, training on a dataset, classifying, and comparing predictions to actual outcomes. The trained model's weights are saved to `weights.txt`, which are then read for future predictions.

## Implementation Challenges
Implementing the machine learning component in Haskell presented a unique challenge due to the absence of extensive machine learning libraries. We designed the model functionally, without relying on class instances, leveraging our experience in machine learning and applying it within the paradigm of functional programming.

## Contributors
- Matthew Kingsbury
- Kaushik Chintam
- Will Ndudirim

---
This project is part of our coursework in the Functional Programming Group.
