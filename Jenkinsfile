pipeline {
    agent any

    stages {
        stage('Build') {
            steps {
                sh 'echo stack --no-terminal --install-ghc test --only-dependencies'
            }
        }
        stage('Test') {
            steps {
                sh 'echo stack --no-terminal test --haddock --no-haddock-deps'
            }
        }
    }
}
