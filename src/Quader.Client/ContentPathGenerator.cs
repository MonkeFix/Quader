

using System.Diagnostics.CodeAnalysis;

// ReSharper disable once CheckNamespace
namespace Nez
{
    /// <summary>
    /// Class that contains the names of all of the files processed by the Pipeline Tool
    /// </summary>
    /// <remarks>
    /// Nez includes a T4 template that will auto-generate the content of this file.
    /// See: https://github.com/prime31/Nez/blob/master/FAQs/ContentManagement.md#auto-generating-content-paths"
    /// </remarks>
	[SuppressMessage("ReSharper", "UnusedMember.Global")]
    [SuppressMessage("ReSharper", "InconsistentNaming")]
    [SuppressMessage("ReSharper", "IdentifierTypo")]
    class Content
    {
		public static class Data
		{
			public static class Board_states
			{
				public const string State_4wide = @"Content/data/board_states/state_4wide.json";
				public const string State_dtcanon = @"Content/data/board_states/state_dtcanon.json";
			}

			public const string Default_piece_settings = @"Content/data/default_piece_settings.json";
		}

		public static class Fonts
		{
			public const string Debug_font = @"Content/fonts/debug_font.fnt";
			public const string Debug_font_0 = @"Content/fonts/debug_font_0.png";
			public const string Font_cfg = @"Content/fonts/font_cfg.bmfc";
			public const string Main_font = @"Content/fonts/main_font.fnt";
			public const string Main_font_0 = @"Content/fonts/main_font_0.png";
			public const string Silkscreen8 = @"Content/fonts/silkscreen8.fnt";
			public const string Silkscreen8_0 = @"Content/fonts/silkscreen8_0.png";
		}

		public static class Skins
		{
			public const string Board_default = @"Content/skins/board_default.png";
			public const string Damage_cell = @"Content/skins/damage_cell.png";
			public const string Damage_meter_background = @"Content/skins/damage_meter_background.png";
			public const string Default = @"Content/skins/default.png";
			public const string Default_2 = @"Content/skins/default_2.png";
			public const string Default_3 = @"Content/skins/default_3.png";
		}


    }
}

