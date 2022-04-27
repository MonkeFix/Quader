using Microsoft.EntityFrameworkCore;
using Quader.WebApi.Models.Users;

namespace Quader.WebApi.Data;

public class QuaderMainContext : DbContext
{
    public DbSet<User> Users { get; private set; } = null!;
    public DbSet<UserRole> UserRoles { get; private set; } = null!;

    public QuaderMainContext(DbContextOptions<QuaderMainContext> options)
        : base(options)
    {
    }

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        base.OnModelCreating(modelBuilder);

        modelBuilder.Entity<User>().ToTable("User");
        modelBuilder.Entity<UserRole>().ToTable("UserRole");
    }
}