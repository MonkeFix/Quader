using System.ComponentModel.DataAnnotations;
using Newtonsoft.Json;

namespace Quader.WebApi.Models.Users;

public class UserRegisterRequest
{
    [JsonRequired]
    [Required(ErrorMessage = "Username is required")]
    public string Username { get; set; } = null!;

    public string Email { get; set; }

    [JsonRequired]
    [Required(ErrorMessage = "Password is required")]
    public string Password { get; set; } = null!;
}