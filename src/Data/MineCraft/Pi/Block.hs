--------------------------------------------------------------------------------
-- |
--  Module      :  Block
--  License     :  Public Domain
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  Haskell 98
--
-- Block handling.
--
-- It probably makes sense for the block types in
-- @Data.MineCraftg.Pi.Types@ - namely @BlockType@ and @BlockData@
-- to be moved into this module.
--
-- See <http://www.minecraftwiki.net/wiki/Data_values_(Pocket_Edition)> and
-- <http://www.minecraftwiki.net/wiki/Pi_Edition_version_history>, although I
-- have not cross-matched and verified all this information.
--
--------------------------------------------------------------------------------

module Data.MineCraft.Pi.Block
    (
      -- * Queries
      getBlock
    , getBlockData
    , getBlocks

      -- * Commands
    , setBlock
    , setBlockData
    , setBlocks
    , setBlocksData

      -- * Blocks
    , showBlock
    , air
    , stone
    , grass
    , dirt
    , cobblestone
    , woodPlanks
    , sapling
    , bedrock
    , water
    , waterStationary
    , lava
    , lavaStationary
    , sand
    , gravel
    , goldOre
    , ironOre
    , coalOre
    , wood
    , leaves
    , glass
    , lapisLazuliOre
    , lapisLazuliBlock
    , sandstone
    , bed
    , cobweb
    , grassTall
    , wool
    , flowerYellow
    , flowerCyan
    , mushroomBrown
    , mushroomRed
    , goldBlock
    , ironBlock
    , stoneSlabDouble
    , stoneSlab
    , brickBlock
    , tnt
    , bookshelf
    , mossStone
    , obsidian
    , torch
    , fire
    , stairsWood
    , chest
    , diamondOre
    , diamondBlock
    , craftingTable
    , farmland
    , furnaceInactive
    , furnaceActive
    , doorWood
    , ladder
    , stairsCobblestone
    , doorIron
    , redstoneOre
    , snow
    , ice
    , snowBlock
    , cactus
    , clay
    , sugarCane
    , fence
    , glowstoneBlock
    , bedrockInvisible
    , stoneBrick
    , glassPane
    , melon
    , fenceGate
    , glowingObsidian
    , netherReactorCore
    ) where

import Control.Monad (liftM)

import Data.Maybe (fromMaybe)
import Data.Word (Word16)

import Data.MineCraft.Pi.Types
import Network.MineCraft.Pi.Client
import Network.MineCraft.Pi.Client.Internal

-- | Block types.
air, stone, grass, dirt, cobblestone,
  woodPlanks, sapling, bedrock, water,
  waterStationary, lava, lavaStationary, sand,
  gravel, goldOre, ironOre, coalOre,
  wood, leaves, glass,
  lapisLazuliOre, lapisLazuliBlock,
  sandstone, bed, cobweb, grassTall, wool,
  flowerYellow, flowerCyan,
  mushroomBrown, mushroomRed,
  goldBlock, ironBlock, stoneSlabDouble, stoneSlab,
  brickBlock, tnt, bookshelf, mossStone, obsidian,
  torch, fire, stairsWood, chest, diamondOre,
  diamondBlock, craftingTable, farmland,
  furnaceInactive, furnaceActive, doorWood, ladder,
  stairsCobblestone, doorIron, redstoneOre,
  snow, ice, snowBlock, cactus, clay, sugarCane,
  fence, glowstoneBlock, bedrockInvisible,
  stoneBrick, glassPane, melon, fenceGate,
  glowingObsidian, netherReactorCore
  :: BlockType

air = BlockType 0
stone = BlockType 1
grass = BlockType 2
dirt = BlockType 3
cobblestone = BlockType 4
woodPlanks = BlockType 5
sapling = BlockType 6
bedrock = BlockType 7
water = BlockType 8
waterStationary = BlockType 9
lava = BlockType 10
lavaStationary = BlockType 11
sand = BlockType 12
gravel = BlockType 13
goldOre = BlockType 14
ironOre = BlockType 15
coalOre = BlockType 16
wood = BlockType 17
leaves = BlockType 18

glass = BlockType 20

lapisLazuliOre = BlockType 21
lapisLazuliBlock = BlockType 22

sandstone = BlockType 24

bed = BlockType 26

cobweb = BlockType 30
grassTall = BlockType 31

wool = BlockType 35

flowerYellow = BlockType 37
flowerCyan = BlockType 38
mushroomBrown = BlockType 39
mushroomRed = BlockType 40
goldBlock = BlockType 41
ironBlock = BlockType 42
stoneSlabDouble = BlockType 43
stoneSlab = BlockType 44
brickBlock = BlockType 45
tnt = BlockType 46
bookshelf = BlockType 47
mossStone = BlockType 48
obsidian = BlockType 49
torch = BlockType 50
fire = BlockType 51

stairsWood = BlockType 53
chest = BlockType 54

diamondOre = BlockType 56
diamondBlock = BlockType 57
craftingTable = BlockType 58 

farmland = BlockType 60
furnaceInactive = BlockType 61
furnaceActive = BlockType 62

doorWood = BlockType 64
ladder = BlockType 65

stairsCobblestone = BlockType 67

doorIron = BlockType 71

redstoneOre = BlockType 73

snow = BlockType 78
ice = BlockType 79
snowBlock = BlockType 80
cactus = BlockType 81
clay = BlockType 82
sugarCane = BlockType 83

fence = BlockType 85

glowstoneBlock = BlockType 89

bedrockInvisible = BlockType 95

stoneBrick = BlockType 98

glassPane = BlockType 102
melon = BlockType 103

fenceGate = BlockType 107

glowingObsidian = BlockType 246
netherReactorCore = BlockType 247

-- For now assume the table is small enough it is not
-- worth using a map.
blockNames :: [(Word16, String)]
blockNames = 
    [ (0, "Air")
    , (1, "Stone")
    , (2, "Grass")
    , (3, "Dirt")
    , (4, "Cobblestone")
    , (5, "Wooden Plank")
    , (6, "Sapling")
    , (7, "BedRock")
    , (8, "Water")
    , (9, "Stationary water")
    , (10, "Lava")
    , (11, "Stationary lava")
    , (12, "Sand")
    , (13, "Gravel")
    , (14, "Gold Ore")
    , (15, "Iron Ore")
    , (16, "Coal Ore")
    , (17, "Wood")
    , (18, "Leaves")
    , (20, "Glass")
    , (21, "Lapis Lazuli Ore")
    , (22, "Lapis Lazuli Block")
    , (24, "Sandstone")
    , (26, "Bed")
    , (30, "Cobweb")
    , (31, "Tall Grass")
    , (35, "Wool")
    , (37, "Yellow Flower")
    , (38, "Cyan Flower")
    , (39, "Brown Mushroom")
    , (40, "Brown Mushroom")
    , (41, "Gold Block")
    , (42, "Iron Block")
    , (43, "Double Stone Slab")
    , (44, "Stone Slab")
    , (45, "Brick Block")
    , (46, "TNT")
    , (47, "Bookshelf")
    , (48, "Moss Stone")
    , (49, "Obsidian")
    , (50, "Torch")
    , (51, "Fire")
    , (53, "Wooden Stairs")
    , (54, "Chest")
    , (56, "Diamond Ore")
    , (57, "Diamond Block")
    , (58, "Crafting Table")
    , (59, "Wheat Seeds") -- valid?
    , (60, "Farmland")
    , (61, "Furnace")
    , (62, "Burning Furnace")
    , (63, "Sign Post")
    , (64, "Wooden Door")
    , (65, "Ladder")
    , (67, "Cobblestone Stairs")
    , (68, "Wall Sign") -- valid?
    , (71, "Iron Door")
    , (73, "Redstone Ore")
    , (74, "Glowing Redstone Ore") -- valid?
    , (78, "Snow")
    , (79, "Ice")
    , (80, "Snow Block")
    , (81, "Cactus")
    , (82, "Clay")
    , (83, "Sugar Cane")
    , (85, "Fence")
    , (87, "Netherrack") -- valid?
    , (89, "Glowstone Block")
    , (95, "Invisible Bedrock")
    , (98, "Stone Brick")
    , (102, "Glass Pane")
    , (103, "Melon")
    , (105, "Melon Stem") -- valid?
    , (107, "Fence Gate")
    , (108, "Brick Stairs") -- valid?
    , (109, "Stone Brick Stairs") -- valid?
    , (112, "Nether Brick") -- valid?
    , (114, "Nether Brick Stairs") -- valid?
    , (128, "Sandstone Stairs") -- valid?
    , (155, "Block of Quartz") -- valid?
    , (156, "Quartz Stairs") -- valid?
    , (245, "Stone Cutter") -- valid?
    , (246, "Glowing Obsidian")
    , (247, "Nether Reactor Core")
    , (249, "Update Game Block") -- valid?
    , (253, "Grass Block (mimic)") -- valid?
    , (254, "Leaves (mysterious)") -- valid?
    , (255, ".name") -- valid?
    ]

-- | Return a name for the block type, or "Unknown block <number>"
--   if unknown.
showBlock :: BlockType -> String
showBlock (BlockType n) =
    fromMaybe ("Unknown block <" ++ show n ++ ">")
              $ lookup n blockNames

-- | What is the block at this position? See also `getBlockData`.
getBlock :: IPos -> MCPI BlockType
getBlock pos = fromMC `liftM` query "world.getBlock" [toMC pos]

-- | What is the block at this position? See also `getBlock`.
getBlockData :: IPos -> MCPI (BlockType, BlockData)
getBlockData pos = fromMC `liftM` query "world.getBlockWithData" [toMC pos]

-- | Get the blocks in the cuboid defined by the start and end positions.
--
getBlocks :: 
    IPos      -- ^ One corner of the cuboid. 
    -> IPos   -- ^ Opposite corner.
    -> MCPI [BlockType] -- ^ The order has not been specified.
getBlocks spos epos = fromMC `liftM`
                      query "world.getBlocks" [toMC spos, toMC epos]

-- | Change the block at the position. See also `setBlockData`.
setBlock :: IPos -> BlockType -> MCPI ()
setBlock pos bt = command "world.setBlock" [toMC pos, toMC bt]

-- | Change the block at the position. See also `setBlock`.
setBlockData :: IPos -> (BlockType, BlockData) -> MCPI ()
setBlockData pos (bt, bd) =
  command "world.setBlock" [toMC pos, toMC bt, toMC bd]

-- | Set all the blocks in the cuboid to the same type. See also
--   `setBlocksData`.
setBlocks :: IPos -> IPos -> BlockType -> MCPI ()
setBlocks spos epos bt =
  command "world.setBlocks" [toMC spos, toMC epos, toMC bt]

-- | Set all the blocks in the cuboid to the same type. See also
--   `setBlocks`.
setBlocksData :: IPos -> IPos -> (BlockType, BlockData) -> MCPI ()
setBlocksData spos epos (bt, bd) =
  command "world.setBlocks" [toMC spos, toMC epos, toMC bt, toMC bd]


