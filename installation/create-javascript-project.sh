#!/bin/bash

# JavaScript/TypeScript Project Template Creator
# Creates a new project with all necessary configuration files

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Get project name
PROJECT_NAME="${1:-my-javascript-project}"

print_status "Creating JavaScript/TypeScript project: $PROJECT_NAME"

# Create project directory
if [ -d "$PROJECT_NAME" ]; then
    print_error "Directory $PROJECT_NAME already exists"
    exit 1
fi

mkdir -p "$PROJECT_NAME"
cd "$PROJECT_NAME"

print_success "Created project directory: $PROJECT_NAME"

# Create package.json
cat > package.json << 'EOF'
{
  "name": "my-javascript-project",
  "version": "1.0.0",
  "description": "Modern JavaScript project with Emacs development setup",
  "main": "dist/index.js",
  "scripts": {
    "build": "tsc",
    "dev": "tsc --watch",
    "start": "node dist/index.js",
    "lint": "eslint src/**/*.{js,ts}",
    "lint:fix": "eslint src/**/*.{js,ts} --fix",
    "format": "prettier --write src/**/*.{js,ts,json}",
    "format:check": "prettier --check src/**/*.{js,ts,json}",
    "type-check": "tsc --noEmit"
  },
  "keywords": ["javascript", "typescript", "emacs"],
  "author": "Your Name",
  "license": "MIT",
  "devDependencies": {
    "@types/node": "^20.0.0",
    "@typescript-eslint/eslint-plugin": "^6.0.0",
    "@typescript-eslint/parser": "^6.0.0",
    "eslint": "^8.0.0",
    "prettier": "^3.0.0",
    "typescript": "^5.0.0"
  }
}
EOF

# Update project name in package.json
sed -i "s/my-javascript-project/$PROJECT_NAME/g" package.json

print_success "Created package.json"

# Create TypeScript configuration
cat > tsconfig.json << 'EOF'
{
  "compilerOptions": {
    "target": "ES2020",
    "module": "ESNext",
    "moduleResolution": "node",
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true,
    "allowJs": true,
    "checkJs": false,
    "declaration": true,
    "outDir": "./dist",
    "rootDir": "./src"
  },
  "include": [
    "src/**/*"
  ],
  "exclude": [
    "node_modules",
    "dist"
  ]
}
EOF

print_success "Created tsconfig.json"

# Create Prettier configuration
cat > .prettierrc << 'EOF'
{
  "semi": true,
  "trailingComma": "es5",
  "singleQuote": true,
  "printWidth": 80,
  "tabWidth": 2,
  "useTabs": false,
  "bracketSpacing": true,
  "arrowParens": "avoid"
}
EOF

print_success "Created .prettierrc"

# Create ESLint configuration
cat > .eslintrc.js << 'EOF'
module.exports = {
  env: {
    browser: true,
    es2021: true,
    node: true,
  },
  extends: [
    'eslint:recommended',
    '@typescript-eslint/recommended',
  ],
  parser: '@typescript-eslint/parser',
  parserOptions: {
    ecmaVersion: 'latest',
    sourceType: 'module',
  },
  plugins: [
    '@typescript-eslint',
  ],
  rules: {
    // Add your custom rules here
    'no-unused-vars': 'warn',
    'no-console': 'warn',
  },
};
EOF

print_success "Created .eslintrc.js"

# Create source directory and sample file
mkdir -p src

cat > src/index.ts << 'EOF'
/**
 * Sample TypeScript file for testing Emacs JavaScript development setup
 */

interface User {
  id: number;
  name: string;
  email: string;
}

class UserManager {
  private users: User[] = [];

  addUser(user: User): void {
    this.users.push(user);
    console.log(`User ${user.name} added successfully`);
  }

  getUserById(id: number): User | undefined {
    return this.users.find(user => user.id === id);
  }

  getAllUsers(): User[] {
    return [...this.users];
  }
}

// Example usage
const userManager = new UserManager();

userManager.addUser({
  id: 1,
  name: 'John Doe',
  email: 'john@example.com',
});

const user = userManager.getUserById(1);
if (user) {
  console.log(`Found user: ${user.name}`);
}

export { UserManager, User };
EOF

print_success "Created src/index.ts"

# Create .gitignore
cat > .gitignore << 'EOF'
# Dependencies
node_modules/

# Build output
dist/

# Logs
*.log
npm-debug.log*

# Environment variables
.env
.env.local

# IDE files
.vscode/
.idea/

# OS files
.DS_Store
Thumbs.db
EOF

print_success "Created .gitignore"

# Create README
cat > README.md << EOF
# $PROJECT_NAME

Modern JavaScript/TypeScript project set up for Emacs development.

## Setup

\`\`\`bash
npm install
\`\`\`

## Scripts

- \`npm run build\` - Compile TypeScript
- \`npm run dev\` - Watch mode compilation
- \`npm run start\` - Run the compiled JavaScript
- \`npm run lint\` - Check code for errors
- \`npm run lint:fix\` - Fix linting errors
- \`npm run format\` - Format code with Prettier
- \`npm run type-check\` - Type check without emitting files

## Emacs Development

This project is configured for modern JavaScript/TypeScript development in Emacs:

- **LSP**: Language Server Protocol support
- **Tree-sitter**: Enhanced syntax highlighting (Emacs 29+)
- **Formatting**: Automatic code formatting with Prettier
- **Linting**: Code quality checks with ESLint

### Key Bindings

- \`C-c l\` - LSP command prefix
- \`M-.\` - Go to definition
- \`M-?\` - Find references
- \`C-c C-d\` - Show documentation
- \`C-c C-r\` - Rename symbol
- \`C-c d\` - Insert JSDoc comment
- \`C-c C-z\` - Switch to Node.js REPL
EOF

print_success "Created README.md"

echo ""
print_success "🎉 Project $PROJECT_NAME created successfully!"
echo ""
print_status "Next steps:"
echo "1. cd $PROJECT_NAME"
echo "2. npm install"
echo "3. npm run build"
echo "4. npm run start"
echo ""
print_status "Open src/index.ts in Emacs to start developing with full LSP support!" 