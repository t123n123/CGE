
let novice = {
    

    triggers = [( fun ev -> ev.type == "Card Played" && ev.card_id = id) , (draw 0)];
}
