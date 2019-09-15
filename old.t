-- Top-level definitions are top-level fields of the view that this module represents
-- Constants are material src tables

type CardType = Lifestyle | Battle

card :: Card
  { name :: String
  , type :: LifeStyle
  , points :: Int }

cards :: [Card]
cards =
  [ { name = "foo"
    , type = Lifestyle
    , points = 3 },
  , { name = "bar"
    , type = Battle
    , points = (-3) } ]

type User = User
  { name :: String }
users :: [User]
users =
  [ { name = "Greg" } ]

type Registration = Registration
  { initiator :: UserId
  , invited :: [UserId]
  , accepted :: [UserId] }

registrations :: [Registration]
registrations = []

-- TODO: need startGame which requires that invited by nonempty

-- TODO: Games should consist of all fully-accepted games without having to take an extra step of anointing them; without
-- that, we have to move ready games into 'games' in some post-update step
-- HMM: wouldn't registration ids be in the same namespace as game ids?  How else to know which games haven't started yet?
readyToStart = filter ready registrations
  where ready { invited = invited, accepted = accepted } | invited == accepted = True
        ready _ = False

type Roster = [UserId]

type Game = Game
  { initiator :: UserId
  , roster :: Roster
  , nextPlayer :: UserId
  , scores :: M.Map UserId Int
  , lifestyleCards :: M.Map UserId [CardId] }

games :: [Games]
--gameIds = [0..length games-1]

-- Actions
startReadyGames :: TMI ()
startReadyGames = do
  let irps = mapM getRosterAndRemove readyToStart
  where getRosterAndRemove (Registration { initiator = initiator, accepted = accepted }) = (initiator, accepted)
  -- This is a straight-up replacement
  readyToStart <-- []
  newGames = mep makeNewGame irps
  games <-- games ++ newGames
  where makeNewGame (initiator, accepted) = Game { initiator = initiator
                                                 , roster = accepted + [initiator]
                                                 , nextPlayer = roster !! 0
                                                 , scores = -- some fold thing
                                                 , lifestyleCards = -- some fold thing }

postUpdateStep :: TMI ()
postUpdateStep
  | length readyToStart > 0 = startReadyGames
