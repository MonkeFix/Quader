using System.IdentityModel.Tokens.Jwt;
using System.Security.Claims;
using System.Text;
using Microsoft.Extensions.Options;
using Microsoft.IdentityModel.Tokens;
using Quader.WebApi.Configuration;
using Quader.WebApi.Models.Users;

namespace Quader.WebApi.Services.Auth;

public class TokenGeneratorService : ITokenGeneratorService
{
    private readonly AppSettings _appSettings;

    public TokenGeneratorService(IOptions<AppSettings> appSettings)
    {
        _appSettings = appSettings.Value;
    }

    public string Generate(User user)
    {
        var tokenHandler = new JwtSecurityTokenHandler();
        var key = Encoding.ASCII.GetBytes(_appSettings.Secret);
        var claims = new[]
        {
            new Claim(ClaimsIdentity.DefaultNameClaimType, user.Username),
            new Claim(ClaimsIdentity.DefaultRoleClaimType, user.Role.RoleName)
        };
        var tokenDescriptor = new SecurityTokenDescriptor
        {
            Subject = new ClaimsIdentity(claims, "Token", ClaimsIdentity.DefaultNameClaimType, ClaimsIdentity.DefaultRoleClaimType),
            // NotBefore = DateTime.UtcNow,
            Expires = DateTime.UtcNow.AddDays(7),
            SigningCredentials = new SigningCredentials(new SymmetricSecurityKey(key), SecurityAlgorithms.HmacSha256Signature),
            Issuer = AppVersionInfo.AppName + " " + AppVersionInfo.BuildVersion,
            Audience = "Quader"
        };
        var token = tokenHandler.CreateToken(tokenDescriptor);

        return tokenHandler.WriteToken(token);
    }
}