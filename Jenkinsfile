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
        changed {
            script {
                if (currentBuild.currentResult == 'FAILURE') { // Other values: SUCCESS, UNSTABLE
                    // Send an email only if the build status has changed from green/unstable to red
                    emailext subject: '$DEFAULT_SUBJECT',
                        body: '$DEFAULT_CONTENT',
                        recipientProviders: [
                            [$class: 'CulpritsRecipientProvider'],
                            [$class: 'DevelopersRecipientProvider'],
                            [$class: 'RequesterRecipientProvider']
                        ], 
                        replyTo: '$DEFAULT_REPLYTO',
                        to: '$DEFAULT_RECIPIENTS'
                }
            }
        }
    }
}
