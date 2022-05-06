using System.Collections.Generic;
using Microsoft.Xna.Framework.Graphics;
using Nez.Textures;
using Quader.Engine;

namespace Quader.Skinning
{
    public class BoardSkin
    {
        private readonly Dictionary<BoardCellType, Sprite> _pieceSpriteMap;
        private readonly List<Sprite> _spriteList;

        public Sprite this[BoardCellType type] => _pieceSpriteMap[type];

        public int CellSize { get; }
        public readonly int Count = 12;

        public BoardSkin(Texture2D skinTexture)
        {
            _pieceSpriteMap = new Dictionary<BoardCellType, Sprite>(Count);
            _spriteList = new List<Sprite>(Count);

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
        }
    }
}