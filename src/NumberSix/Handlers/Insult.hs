--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module NumberSix.Handlers.Insult
    ( handler
    , randomInsult
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Monad.Trans (liftIO)
import           Data.Text           (Text)


--------------------------------------------------------------------------------
import           NumberSix.Bang
import           NumberSix.Irc
import           NumberSix.Util


--------------------------------------------------------------------------------
handler :: UninitializedHandler
handler = makeBangHandler "Insult" ["!insult"] $ \user ->
    (user <> ": " <>) <$> liftIO randomInsult


--------------------------------------------------------------------------------
randomInsult :: IO Text
randomInsult = randomElement insults


--------------------------------------------------------------------------------
insults :: [Text]
insults =
    [ "fart smeller"
    , "I can't tell if I'm talking to your face or your asshole"
    , "dickweed"
    , "buttpirate"
    , "douchenozzle"
    , "bitch"
    , "dick"
    , "jackass"
    , "turtledick"
    , "shut your cockholster"
    , "fudgepacker"
    , "I think you'd be in hufflepuff"
    , "wanker"
    , "dumbshit"
    , "suck a bag of dicks"
    , "before I met you I was pro-life"
    , "shitbag"
    , "mouth breather"
    , "I hope something you love catches on fire"
    , "pissant"
    , "if you die, I'm not going to your funeral"
    , "if you were any less intelligent I would have to water you twice a week"
    , "you look like a before picture"
    , "your mother has two cunts and you're one of them"
    , "dumbass"
    , "you're not very nice and I don't like you"
    , "son of a bitch"
    , "shithead"
    , "how appropriate, you fight like a cow"
    , "calling you stupid is an insult to stupid people"
    , "twat"
    , "your mother fucks for bricks so she can build your sister a whore house"
    , "fuckface"
    , "horsefucker"
    , "everyone who ever loved you was wrong"
    , "ho bag"
    , "you are a sad, lonely little man, and you have my pity"
    , "why don't you go outside and play hide-and-go-fuck-yourself?"
    , "clitsquiggle"
    , "puff"
    , "dickhead"
    , "douche canoe"
    , "asshat"
    , "you're about as useful as Anne Frank's drum kit"
    , "you have the most depressing smile"
    , "cocksucker"
    , "dipshit"
    , "pussy"
    , "your muff is cabbage"
    , "you sir, are unremarkable and unmemorable in every respect"
    , "cum dumpster"
    , "niggerfaggot"
    , "ball licker"
    , "thundercunt"
    , "you're not pretty enough to be this much of a bitch"
    , "I refuse to have a battle of wits with an unarmed man"
    , "gaywad"
    , "skank"
    , "bastard"
    , "with a face like yours, I'd be very careful who and what I make fun of"
    , "I hope you step on a Lego"
    , "I would slap you, but I don't want to get slut on my hand"
    , "prick"
    , "the sound of your piss hitting the urinal is feminine"
    , "I had sex with your wife"
    , "turd sandwich"
    , "jive turkey"
    , "your mother was a hamster and your father smelt of elderberries"
    , "I hope your asshole grows taste buds"
    , "bitch, you dumb"
    , "you are the opposite of batman"
    , "if your vagina had a password, it would be \"password\""
    , "I'd call you a cunt, but you lack the warmth and the depth"
    , "fucktard"
    , "you're the AT&T of people"
    , "I worship the ground that awaits your grave"
    , "history will judge me harshly for not having killed you"
    ]
