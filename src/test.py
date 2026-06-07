game = {"test": 1}
stack = []

# how to detect events?
# Option 1
# - broadcast event to all listeners
# - filter and handle each separately
# pro: straighforward implementation
# con: each event requires a separate type definition
#
# Option 2:
# - an event is triggered by a piece of code
# - make sure the code that handles the event is "aware" of the "handler"
# pro: no need for separate event types
# con: self-modifying code is difficult to reason about
# how to execute option 2?
# - compose a game state (basic_state + state_modifiers)
# - state contains the "transformer" functions used to modify the game state
# - transformer functions can be modified at runtime to encode "listeners"
#
# Combined static effects:
# - Effect 1: your creatures have +1/+1
# - Effect 2: when a creature with 4 or more attack enters, draw a card
#
# Play a 3/3 creature -> +1/+1 modifier applies -> draw a card
# Play a 3/3 creature -> don't draw a card -> becomes a 4/4 creature afterwards
# Timing of event modifiers matters! How to handle this cleanly (and intuitively)?


# Represent chain of events as a stack of tagged "functions"
# Effects manipulate the whole chain of effects, not just the tip of the stack
#
# wording differences:
# whenever you would do "A", do "B" - "B" happens before "A"
# when you do "A" do "B" - "B" happens after "A"
#
# Subscriber model -> requires subscribing for specific events -> source needs to dispatch all possible hooks
# Can we detect events without adding event types?
#


def end_turn(game):
    game.append(end_turn)
    return game


def add_one(game):
    game["test"] += 1
    return game


stack.append(end_turn)
