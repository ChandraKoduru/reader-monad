module HasTypeClassPattern where

import Data.Text

--https://hackernoon.com/the-has-type-class-pattern-ca12adab70ae

data Scene = Scene
  { backgroundImage :: Image
  , characters  :: [Character]
  , bewilderedTourist :: Maybe Character
  , objects :: [Either Rock WoodenCrate]
  }


data Character = Character
  { hat :: Maybe DamageArray
  , head :: DamageArray
  , torso :: DamageArray
  , legs :: DamageArray
  , shoes :: Maybe DamageArray
  }

data DamageArray  = DamageArray
  { noDamage :: Image
  , someDamage :: Image
  , excessiveDamage :: Image
  }

data Rock = Rock
  { weight :: Double
  , rockImage :: Image
  }

data WoodenCrate = WoodenCrate
  { strength :: Double
  , woodenCrateImage :: DamageArray
  }

type Image = Text

collectImages :: Scene -> Set Image
collectImages Scene {..}
  = singleton backgroundImage
  <> mconcat (map collectCharacterImages characters)
  <> maybe mempty collectCharacterImages bewilderedTourist
  <> mconcat (map (either (singleton . collectRockImage)
                          collectWoodenCrateImages)
                  objects)

collectCharacterImages :: Character -> Set Image
collectCharacterImages Character {..}
  = maybe mempty collectDamageArrayImages hat
  <> collectDamageArrayImages head
  <> collectDamageArrayImages torso
  <> collectDamageArrayImages legs
  <> may mempty collectDamageArrayImages shoes

collectDamageArrayImages :: DamageArray -> Set Image
collectDamageArrayImages DamageArray {..} =
  fromList [ noDamage
           , someDamage
           , excessiveDamage
           ]

collectRockImage :: Rock -> Image
collectRockImage Rock {..} = rockImage

collectWoodenCrateImages :: WoodenCrate -> Set Image
collectWoodenCrateImages WoodenCrate {..} =
  collectDamageArrayImages woodenCrateImage

-- Using Has type class pattern

class HasImages a where
  images :: a -> Set Image

instance HasImages a => HasImages [a] where
  images xs = foldr (\x accum -> images x <> accum)
                    mempty
                    xs

instance HasImages a => HasImages (Maybe a) where
  images x = maybe [] images x

instance (HasImages a, HasImages b) => HasImages (Either a b) where
  images x = either images images x

instance HasImages Scene where
  images Scene {..}
    = singleton backgroundImage
    <> images characters
    <> images bewilderedTourist
    <> images objects

instance HasImages Character where
  images Character {..}
    = images hat
    <> images head
    <> images torso
    <> images legs
    <> images shoes

instance HasImages DamageArray where
  images DamageArray {..} = fromList
    [ noDamage
    , someDamage
    , excessiveDamage
    ]

instance HasImages Rock where
  images Rock {..} = singleton rockImage

instance HasImages WoodenCrate where
  images WoodenCrate {..} = images woodenCrateImage

