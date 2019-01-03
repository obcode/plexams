pipeline {
    agent none
    stages {
        stage('Build') {
            agent {
                docker {
                    image 'haskell:8.4.4'
                    args '-v /home/jenkins/.stack:/home/jenkins/.stack'
                }
            }
            steps {
                sh 'stack --no-terminal test --only-dependencies'
                sh 'stack --no-terminal test'
            }
        }
    }
}
