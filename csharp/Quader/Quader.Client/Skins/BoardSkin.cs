using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Nez.Textures;
using Nez;
using Nez.BitmapFonts;
using Quader.Client.Extensions;
using Quader.Engine.Boards;
using Quader.Engine.Pieces;

namespace Quader.Client.Skins;

public class BoardSkin
{
    private readonly Dictionary<CellType, Sprite> _pieceSpriteMap;
    private readonly List<Sprite> _spriteList;

    public Sprite this[CellType type] => _pieceSpriteMap[type];

    public static readonly IEnumerable<PieceType> AvailablePieces =
    [
        PieceType.I,
        PieceType.J,
        PieceType.L,
        PieceType.S,
        PieceType.Z,
        PieceType.O,
        PieceType.T
    ];

    public Texture2D BoardTexture { get; }
    public Sprite GhostSprite { get; private set; } = null!;

    public BitmapFont MainFont { get; }
    public BitmapFont DebugFont { get; }
    public BitmapFont SilkscreenFont { get; }

    public Dictionary<PieceType, RenderTarget2D> PieceTextures { get; }

    public BoardSkinTable Table { get; }

    public int CellSize { get; }
    public readonly int Count = 12;

    public BoardSkin(
        Texture2D skinTexture,
        Texture2D boardTexture,
        BitmapFont mainFont,
        BitmapFont debugFont,
        BitmapFont silkscreenFont
    )
    {
        BoardTexture = boardTexture;
        MainFont = mainFont;
        DebugFont = debugFont;
        SilkscreenFont = silkscreenFont;
        _pieceSpriteMap = new Dictionary<CellType, Sprite>(Count);
        _spriteList = new List<Sprite>(Count);

        PieceTextures = new Dictionary<PieceType, RenderTarget2D>();

        var size = skinTexture.Height;
        CellSize = size;

        Table = BoardSkinTable.Default;

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

        _pieceSpriteMap[CellType.Z] = z;
        _pieceSpriteMap[CellType.L] = l;
        _pieceSpriteMap[CellType.I] = i;
        _pieceSpriteMap[CellType.O] = o;
        _pieceSpriteMap[CellType.J] = j;
        _pieceSpriteMap[CellType.S] = s;
        _pieceSpriteMap[CellType.T] = t;
        _pieceSpriteMap[CellType.Garbage] = garbage;
        _pieceSpriteMap[CellType.Solid] = solid;

        foreach (var pieceType in AvailablePieces)
        {
            var bounds = PieceHelpers.CalcBounds(
                PieceHelpers.GetPointsForPiece(pieceType, RotationState.Initial), 0, 0);
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
            //var points = PieceUtils.GetPiecePointsByType(type);
            var points = PieceHelpers.GetPointsForPiece(type, RotationState.Initial);
            rt.RenderFrom(
                batcher,
                b =>
                    RenderPiece(b, points, PieceHelpers.PieceTypeToCellType(type))
            );
        }
    }

    private void RenderPiece(Batcher batcher, Quader.Engine.Primitives.Point[] points,
        CellType cellType)
    {
        batcher.Begin();
        foreach (var p in points)
        {
            var pos = new Vector2((p.X + 1) * CellSize, (p.Y + 1) * CellSize);

            if (cellType == CellType.I)
                pos.X += CellSize;

            var sprite = this[cellType];


            batcher.Draw(
                sprite.Texture2D,
                pos,
                sprite.SourceRect,
                Color.White
            );
        }

        batcher.End();
    }
}