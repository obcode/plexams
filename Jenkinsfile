pipeline {
    agent any

    stages {
        stage('Build') {
            steps {
                sh 'stack --no-terminal --install-ghc test --only-dependencies'
            }
        }
        stage('Test') {
            steps {
                sh 'stack --no-terminal test --haddock --no-haddock-deps'
            }
        }
    }
}
