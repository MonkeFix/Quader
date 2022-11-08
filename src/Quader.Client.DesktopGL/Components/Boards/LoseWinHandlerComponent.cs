using System;
using Nez;
using Nez.UI;
using Quader.Engine;
using Quader.Skinning;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;
using Quader.Debugging.Logging;
using Quader.Engine.Replays;

namespace Quader.Components.Boards
{
    public class LoseWinHandlerComponent : RenderableComponent, IBoardComponent, IBoardToggleable
    {
        enum WonOrLost { None, Won, Lost }

        public Board Board { get; }

        public override float Width { get; }
        public override float Height { get; }

        public bool IsEnded { get; private set; }

        private WonOrLost _state = WonOrLost.None;

        private BoardSkin _skin;

        private Vector2 _offset;

        private BoardManagerComponent _boardManager;
        
        private readonly ILogger _logger = LoggerFactory.GetLogger<LoseWinHandlerComponent>();

        public LoseWinHandlerComponent(Board board, BoardManagerComponent boardManager)
        {
            Board = board;
            _boardManager = boardManager;

            Width = 500;
            Height = 600;

            _skin = Core.Services.GetService<Skin>().Get<BoardSkin>();

            Board.PieceCannotBeSpawned += BoardOnPieceCannotBeSpawned;
        }

        private void BoardOnPieceCannotBeSpawned(object? sender, EventArgs e)
        {
            _state = WonOrLost.Lost;
        }

        public override void OnAddedToEntity()
        {
            _offset = new Vector2(Entity.Position.X + 140, Entity.Position.Y + Board.Height * _skin.CellSize / 2f - _skin.CellSize);

            LayerDepth = 0;
        }

        public override void Render(Batcher batcher, Camera camera)
        {
            if (_boardManager.State == GameState.GameTimer)
            {
                var timeLeft = (int)_boardManager.CurrentTimerValue;
                var timeStr = timeLeft == 0 ? "GO" : timeLeft.ToString();
                batcher.DrawString(_skin.MainFont, timeStr, _offset, Color.White, 0, Vector2.Zero, Vector2.One, SpriteEffects.None, 0f);
            }
            else if (_state == WonOrLost.Won)
            {
                batcher.DrawString(_skin.MainFont, "WIN", _offset, Color.White, 0, Vector2.Zero, Vector2.One, SpriteEffects.None, 0f);
            }
            else if (_state == WonOrLost.Lost)
            {       
                batcher.DrawString(_skin.MainFont, "LOSS", _offset, Color.White, 0, Vector2.Zero, Vector2.One, SpriteEffects.None, 0f);
            }
        }

        public void Enable()
        {
            IsEnded = false;
            _state = WonOrLost.None;
            _logger.Trace($"Enabling. New State: {_state}");
        }

        public void Disable()
        {
            IsEnded = true;
            if (_state == WonOrLost.None)
                _state = WonOrLost.Won;
            _logger.Trace($"Disabling. New state: {_state}");
        }
    }
}