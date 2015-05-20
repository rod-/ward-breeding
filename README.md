WarDragons (an iOS game developed by PocketGames) has at least 71 breedable dragons which interact with one another in many different ways.  By analysis of the csv files located within the app, I have discovered the logic used to determine 
what breeding combinations produce which output dragons at what odds.  There are 1944 valid pairings of these dragons - too many to easily interpret.  This shiny application enables you to make practical decisions based on your available options without spending time sifting through all 1944 pairings.

---
  FirstDragon SecondDragon     First Second Third Fourth Fifth Sixth
1       Draco    Leviathan Leviathan  Draco  <NA>   <NA>  <NA>  <NA>
---

This is an example output row from the mating data frame.  FirstDragon and SecondDragon are the two dragons you pair, while First through Sixth are the Six possible outcomes (NA if there are fewer than six outcomes).
The individual odds of rolling each are available, but hidden in this view.  The sum of those odds that are beneficial to the player (containing a new egg) are calculated, and results sorted for that purpose.

The UI allows you to input your available dragons.  You can choose your completed colors with checkboxes and type individual names, with drop-down boxes verifying your choices.  The list of names in the incomplete list is populated using a third text box, telling you to input any incomplete colors.  As you finish your input, the algorithm reactively runs and calculates the best combination of the dragons you have input - with the eventual aim of producing new dragons efficiently.

