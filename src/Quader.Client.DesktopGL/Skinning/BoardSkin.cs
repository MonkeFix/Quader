using System.Collections.Generic;
using System.Linq;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez;
using Nez.BitmapFonts;
using Nez.Textures;
using Quader.Engine;
using Quader.Engine.Pieces;
using Quader.Utils;

namespace Quader.Skinning
{
    public class BoardSkin
    {
        private readonly Dictionary<BoardCellType, Sprite> _pieceSpriteMap;
        private readonly List<Sprite> _spriteList;

        public Sprite this[BoardCellType type] => _pieceSpriteMap[type];

        public static readonly PieceType[] AvailablePieces = new PieceType[]
        {
            PieceType.I,
            PieceType.J,
            PieceType.L,
            PieceType.S,
            PieceType.Z,
            PieceType.O,
            PieceType.T
        };

        public Texture2D BoardTexture { get; }
        public Sprite GhostSprite { get; private set; }

        public BitmapFont MainFont { get; }
        public BitmapFont DebugFont { get; }

        public Dictionary<PieceType, RenderTarget2D> PieceTextures { get; }

        public int CellSize { get; }
        public readonly int Count = 12;

        public BoardSkin(
            Texture2D skinTexture,
            Texture2D boardTexture, 
            BitmapFont mainFont,
            BitmapFont debugFont
            )
        {
            BoardTexture = boardTexture;
            MainFont = mainFont;
            DebugFont = debugFont;
            _pieceSpriteMap = new Dictionary<BoardCellType, Sprite>(Count);
            _spriteList = new List<Sprite>(Count);

            PieceTextures = new Dictionary<PieceType, RenderTarget2D>();

            var size = skinTexture.Height;
            CellSize = size;

            _spriteList = Sprite.SpritesFromAtlas(skinTexture, CellSize, CellSize, 0, Count);

            InitializeFromList();
        }

        private void InitializeFromList()
        {
            var z = _spriteList[0];
            var l = _spriteList[1];
            var o = _spriteList[2];
            var s = _spriteList[3];
            var i = _spriteList[4];
            var j = _spriteList[5];
            var t = _spriteList[6];

            GhostSprite = _spriteList[7];
            var solid = _spriteList[8];
            var garbage = _spriteList[9];

            var fail = _spriteList[11];

            _pieceSpriteMap[BoardCellType.Z] = z;
            _pieceSpriteMap[BoardCellType.L] = l;
            _pieceSpriteMap[BoardCellType.I] = i;
            _pieceSpriteMap[BoardCellType.O] = o;
            _pieceSpriteMap[BoardCellType.J] = j;
            _pieceSpriteMap[BoardCellType.S] = s;
            _pieceSpriteMap[BoardCellType.T] = t;
            _pieceSpriteMap[BoardCellType.Garbage] = garbage;
            _pieceSpriteMap[BoardCellType.Solid] = solid;
            _pieceSpriteMap[BoardCellType.Failing] = fail;

            foreach (var pieceType in AvailablePieces)
            {
                var bounds = PieceBase.GetBounds(PieceUtils.GetPiecePointsByType(pieceType), 0, 0);
                PieceTextures[pieceType] = //RenderTarget.Create(512, 512);
                    RenderTarget.Create(bounds.Width * CellSize, bounds.Height * CellSize);
            }

            Render(Graphics.Instance.Batcher);
        }

        /// <summary>
        /// Renders all the pieces into textures to increase performance. Meant to be called once the time the Batcher is ready
        /// </summary>
        /// <param name="batcher"></param>
        public void Render(Batcher batcher)
        {
            foreach (var type in AvailablePieces)
            {
                var rt = PieceTextures[type];
                var points = PieceUtils.GetPiecePointsByType(type);
                rt.RenderFrom(
                    batcher,
                    b =>
                        RenderPiece(b, points, PieceUtils.GetBoardCellTypeByPieceType(type))
                );
            }
        }

        private void RenderPiece(Batcher batcher, Point[] points, BoardCellType cellType)
        {
            foreach (var p in points)
            {
                var pos = new Vector2((p.X+1) * CellSize, (p.Y+1) * CellSize);

                if (cellType == BoardCellType.I)
                    pos.X += CellSize;

                var sprite = this[cellType];

                
                batcher.Draw(
                    sprite.Texture2D,
                    pos,
                    sprite.SourceRect,
                    Color.White
                );
            }
        }
    }
}