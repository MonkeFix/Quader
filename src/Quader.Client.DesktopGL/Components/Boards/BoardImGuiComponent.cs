using System.Linq;
using ImGuiNET;
using Nez;
using Nez.ImGuiTools;
using Quader.Engine;
using Quader.Engine.Pieces;

namespace Quader.Components.Boards
{
    public class BoardImGuiComponent : Component
    {
        public Board Board { get; }

        public BoardImGuiComponent(Board board)
        {
            Board = board;
        }

        public override void OnAddedToEntity()
        {
            Core.GetGlobalManager<ImGuiManager>().RegisterDrawCommand(ImGuiDraw);
        }

        private void ImGuiDraw()
        {
            ImGui.Begin("Piece Handling");

            ImGui.Text($"Time Data:");

            var vals = GlobalTimeManager.TimeData.Values;
            foreach (var val in vals)
            {
                ImGui.Text(val.ToString());
            }

            var totalLast = vals.Select(data => data.LastTime);
            var totalMean = vals.Select(data => data._average);

            double tl = 0, tm = 0;

            foreach (var l in totalLast)
            {
                tl += l.TotalMilliseconds;
            }

            foreach (var timeSpan in totalMean)
            {
                tm += timeSpan;
            }

            ImGui.Text($"Total: Last: {tl:F6}, Mean: {tm:F6}");

            ImGui.Separator();

            ImGui.Text($"Board Size: {Board.Width}x{Board.Height}");
            if (ImGui.Button("RESET BOARD"))
                Board.Reset();

            ImGui.Separator();

            if (Core.DebugRenderEnabled)
            {
                ImGui.Checkbox("Draw Piece Origin", ref SharedSettings.DrawPieceOrigin);
                ImGui.Checkbox("Draw Bounding Box", ref SharedSettings.DrawPieceBoundingBox);
                ImGui.Checkbox("Draw Piece Test Queue", ref SharedSettings.DrawPieceRotationTests);
            }
            ImGui.Separator();

            var c = Board.CurrentPiece;
            {
                ImGui.Text($"Current Piece: {c.Type}, Position: ({c.X},{c.Y})");
                ImGui.Text($"Bounds: {c.Bounds}");
                ImGui.Text($"Current Rotation: {c.CurrentRotation}");
                //ImGui.Text($"Current Rotation:");
            }

            ImGui.Separator();

            ImGui.Text($"Test Queue Size: {Board.TestQueue.Count}");
            if (ImGui.Button("Queue Next"))
            {
                if (Board.TestQueue.Count > 0)
                {
                    var p = Board.TestQueue.Dequeue();
                    SharedSettings.CurrentTest = p;
                }
                else
                {
                    SharedSettings.CurrentTest = null;
                }
            }

            ImGui.Separator();

            if (ImGui.Button("Spawn I"))
                Board.PushPiece(PieceType.I);
            ImGui.SameLine();
            if (ImGui.Button("Spawn J"))
                Board.PushPiece(PieceType.J);
            if (ImGui.Button("Spawn L"))
                Board.PushPiece(PieceType.L);
            ImGui.SameLine();
            if (ImGui.Button("Spawn T"))
                Board.PushPiece(PieceType.T);
            if (ImGui.Button("Spawn S"))
                Board.PushPiece(PieceType.S);
            ImGui.SameLine();
            if (ImGui.Button("Spawn Z"))
                Board.PushPiece(PieceType.Z);
            if (ImGui.Button("Spawn O"))
                Board.PushPiece(PieceType.O);

            ImGui.End();
        }
    }
}