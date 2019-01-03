pipeline {
    agent none
    stages {
        stage('Build') {
            agent {
                docker {
                    image 'obraun/fun-jenkins'
                    args '-v /home/jenkins/.stack:/home/jenkins/.stack'
                }
            }
            steps {
                sh 'stack --no-terminal test --only-dependencies'
                sh 'stack --no-terminal test'
            }
        }
    }
    post {
        always {
            emailext body: 'A Test EMail', recipientProviders: [[$class: 'DevelopersRecipientProvider'], [$class: 'RequesterRecipientProvider']], subject: 'Test'
        }
    }
}
